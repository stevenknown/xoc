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

namespace xoc {

//
//START MDID
//
void MDId2MD::dump(Region const* rg) const
{
    if (!rg->isLogMgrInit()) { return; }
    for (VecIdx i = 0; i <= get_last_idx(); i++) {
        MD * md = Vector<MD*>::get(i);
        if (md == nullptr) { continue; }
        ASSERT0(MD_id(md) == (MDIdx)i);
        prt(rg, "%d,", i);
    }
}
//END MDID


//
//START MD
//
//Return true if current md may cover 'm', such as:
//current md: |-...-----...---|
//m:            |---...-|
bool MD::is_may_cover(MD const* m) const
{
    ASSERT0(m && this != m);
    if (get_base() != m->get_base()) {
        return false;
    }
    if (MD_ty(this) == MD_UNBOUND) {
        return true;
    }
    if (MD_ty(m) == MD_UNBOUND ||
        MD_ty(this) == MD_RANGE ||
        MD_ty(m) == MD_RANGE) {
        return false;
    }
    if ((MD_ofst(this) <= MD_ofst(m)) &&
        (MD_ofst(this) + MD_size(this) >= MD_ofst(m) + MD_size(m))) {
        return true;
    }
    return false;
}


//Return true if current md exactly cover 'm', such as:
//CASE1:
//  current md: |-------|
//  m:            |----|
//CASE2:
//  current md: |---------|
//  m(range):     |--..--|
bool MD::is_exact_cover(MD const* m) const
{
    ASSERT0(m);
    //Avoid extra judgement of is_exact() for given non-exact MD.
    //ASSERT0(this != m);
    if (get_base() != m->get_base() ||
        !is_exact() ||
        (!m->is_exact() && !m->is_range())) {
        return false;
    }
    return ((MD_ofst(this) <= MD_ofst(m)) &&
           (MD_ofst(this) + MD_size(this) >= MD_ofst(m) + MD_size(m)));
}


//Return true if current md intersect but may be not cover 'm', such as:
//current md: |-------|
//m:            |-------|
bool MD::is_overlap(MD const* m) const
{
    ASSERT0(m && this != m);
    //TBD: Does it necessary to judge if either current
    //MD or input MD is FULL_MEM?
    //As we observed, passes that utilize MD relationship add
    //MD2 to accroding IR's MDSet, which can keep global variables
    //and MD2 dependence.
    //e.g: g=10, #mustdef=MD10, maydef={MD2, MD10}, g is global variable that
    //           #represented in Program Region.
    //     foo(); #maydef={MD2, MD10}
    //if (MD_id(m) == MD_FULL_MEM || MD_id(this) == MD_FULL_MEM) {
    //    return true;
    //}

    //It is also unnecessary to judge HEAP_MD relationship if MD is FULL_MEM.
    //if (MD_id(m) == MD_HEAP_MEM && MD_id(this) == MD_FULL_MEM)
    //{ return true; }

    if (MD_base(m) != MD_base(this)) { return false; }
    if (MD_ty(m) == MD_UNBOUND || MD_ty(this) == MD_UNBOUND) {
        return true;
    }
    //MD is exact or ranged.
    return !(((MD_ofst(m) + MD_size(m)) <= MD_ofst(this)) ||
             ((MD_ofst(this) + MD_size(this)) <= MD_ofst(m)));
}


CHAR * MD::dump(StrBuf & buf, TypeMgr * dm) const
{
    buf.strcat("MD%d -- base:", MD_id(this));
    ASSERT0(MD_base(this) != nullptr);
    MD_base(this)->dump(buf, dm);
    CHAR const* ofstfmt = getUIntFormat(false);
    TMWORD lofst = MD_ofst(this);
    if (MD_ty(this) == MD_EXACT) {
        StrBuf fmt(16);
        fmt.strcat(" -- ofst:%s -- size:%s", ofstfmt, ofstfmt);
        buf.strcat(fmt.buf, lofst, MD_size(this));
    } else if (MD_ty(this) == MD_RANGE) {
        StrBuf fmt(16);
        fmt.strcat(" -- start:%s -- end:%s", ofstfmt, ofstfmt);
        buf.strcat(fmt.buf, lofst, lofst + MD_size(this));
        buf.strcat(" -- range");
    } else {
        buf.strcat(" -- ofst:unbound");
    }
    return buf.buf;
}


void MD::dump(TypeMgr * dm) const
{
    if (!dm->getRegionMgr()->getLogMgr()->is_init()) { return; }
    StrBuf buf(64);
    note(dm->getRegionMgr(), "\n%s", dump(buf, dm));
}
//END MD


//
//START MDSet
//
//Get unique MD that is not fake memory object,
//but its offset might be invalid.
//Note the MDSet can only contain one element.
//Return the effect MD if found, otherwise return nullptr.
MD * MDSet::get_effect_md(MDSystem * ms) const
{
    ASSERT0(ms);
    if (get_elem_count() != 1) {
        return nullptr;
    }
    MDSetIter iter;
    MD * md = ms->getMD(get_first(&iter));
    ASSERT0(md != nullptr);
    if (md->get_base()->is_fake()) {
        return nullptr;
    }
    return md;
}


void MDSet::bunion(MDIdx mdid, DefMiscBitSetMgr & mbsmgr)
{
    //TBD: Does it necessary to judge if either current
    //MD or input MD is FULL_MEM?
    //As we observed, passes that utilize MD relationship add
    //MD2 to accroding IR's MDSet, which can keep global variables
    //and MD2 dependence.
    //e.g: g=10, #mustdef=MD10, maydef={MD2, MD10}, g is global variable that
    //           #represented in Program Region.
    //     foo(); #maydef={MD2, MD10}
    //if (mdid == MD_FULL_MEM) {
    //    clean(mbsmgr);
    //    DefSBitSetCore::bunion(MD_FULL_MEM, mbsmgr);
    //    return;
    //}
    //if (DefSBitSetCore::is_contain(MD_FULL_MEM)) {
    //    ASSERT0(DefSBitSetCore::get_elem_count() == 1);
    //    return;
    //}

    DefSBitSetCore::bunion(mdid, mbsmgr);
}


//Return true current set is equivalent to mds, and every element
//in set is exact.
bool MDSet::is_exact_equal(MDSet const& mds, MDSystem const* ms) const
{
    ASSERT0(ms);
    UINT count = 0;
    INT md1 = -1;
    MDSetIter iter;
    MDSystem * pms = const_cast<MDSystem*>(ms);
    for (INT i = get_first(&iter); i != -1; i = get_next(i, &iter)) {
        if (!pms->getMD(i)->is_exact()) {
            return false;
        }
        md1 = i;
        count++;
        if (count > 1) {
            //If the number of MD more than one, the Alias analysis
            //might compute the consevative solution.
            return false;
        }
    }

    count = 0;
    INT md2 = -1;
    for (INT i = mds.get_first(&iter); i != -1; i = get_next(i, &iter)) {
        if (!pms->getMD(i)->is_exact()) {
            return false;
        }
        md2 = i;
        count++;
        if (count > 1) {
            //If the number of MD more than one,
            //the Alias analysis might compute the consevative solution.
            return false;
        }
    }
    return md1 == md2;
}


bool MDSet::is_contain_only_exact_and_str(MDSystem const* ms) const
{
    ASSERT0(ms);
    MDSetIter iter;
    MDSystem * pms = const_cast<MDSystem*>(ms);
    for (INT i = get_first(&iter); i != -1; i = get_next(i, &iter)) {
        MD * tmd = pms->getMD(i);
        ASSERT0(tmd != nullptr);
        if (!tmd->is_exact() && !MD_base(tmd)->is_string()) {
            return false;
        }
    }
    return true;
}


bool MDSet::is_contain_inexact(MDSystem const* ms) const
{
    ASSERT0(ms);
    MDSetIter iter;
    MDSystem * pms = const_cast<MDSystem*>(ms);
    for (INT i = get_first(&iter); i != -1; i = get_next(i, &iter)) {
        MD * tmd = pms->getMD(i);
        ASSERT0(tmd != nullptr);

        //TBD: Does it necessary to judge if either current
        //MD or input MD is FULL_MEM?
        //As we observed, passes that utilize MD relationship add
        //MD2 to accroding IR's MDSet, which can keep global variables
        //and MD2 dependence.
        //e.g: g=10,  #mustdef=MD10, maydef={MD2, MD10}, g is global
        //            #variable that represented in Program Region.
        //     foo(); #maydef={MD2, MD10}
        //if (MD_id(tmd) == MD_FULL_MEM) {
        //    return true;
        //}

        if (!tmd->is_exact()) {
            return true;
        }
    }
    return false;
}


static bool is_contain_by_global_delegate(MDSet const* set, MD const* md)
{
    if (!md->is_global()) { return false; }
    if (!set->is_contain_pure(MD_GLOBAL_VAR)) { return false; }
    if (md->id() == MD_IMPORT_VAR) {
        //IMPORT_VAR is proxy MD, thus GLOBAL_VAR should not contain it.
        return false;
    }
    return true;
}


static bool is_contain_by_import_delegate(MDSet const* set, MD const* md,
                                          Region const* rg)
{
    ASSERT0(rg);
    if (!set->is_contain_pure(MD_IMPORT_VAR)) { return false; }
    if (MDSystem::isLocalDelegate(md->id())) { return false; }
    ASSERT0(md->is_local());
    return rg->isRegionVAR(md->get_base());
}


//md: is local variable.
static bool is_contain_by_local_delegate(MDSet const* set)
{
    return set->is_contain_pure(MD_LOCAL_VAR);
}


//md: is local variable.
static bool is_contain_by_local_may_alias_delegate(MDSet const* set)
{
    return set->is_contain_pure(MD_LOCAL_MAY_ALIAS);
}


bool MDSet::is_contain_by_delegate(MD const* md, Region const* rg) const
{
    //TBD: Does it necessary to judge if either current
    //MDSet or input MD is FULL_MEM?
    //As we observed, passes that utilize MD relationship add
    //MD2 to accroding IR's MDSet, which can keep global variables
    //and MD2 dependence.
    //e.g: g=10, #mustdef=MD10, maydef={MD2, MD10}, g is global variable that
    //           #represented in Program Region.
    //     foo(); #maydef={MD2, MD10}
    //if (!is_contain_pure(MD_FULL_MEM)) {
    //    ASSERT0(!DefSBitSetCore::is_empty());
    //    return true;
    //}
    ASSERT0(!is_contain_pure(MD_FULL_MEM));
    if (md->is_global()) {
        return is_contain_by_global_delegate(this, md);
    }
    if (is_contain_by_import_delegate(this, md, rg)) {
        return true;
    }
    ASSERT0(md->is_local());
    if (is_contain_by_local_delegate(this)) {
        return true;
    }
    if (is_contain_by_local_may_alias_delegate(this)) {
        return true;
    }
    return false;
}


//Return true if set contained md.
bool MDSet::is_contain(MD const* md, Region const* current_rg) const
{
    if (is_contain_by_delegate(md, current_rg)) {
        return true;
    }
    return DefSBitSetCore::is_contain(MD_id(md));
}


//Return true if md is overlap with the elements in set.
bool MDSet::is_overlap(MD const* md, Region const* current_rg) const
{
    return is_contain(md, current_rg);
}


//Return true if 'md' overlapped with element in current MDSet.
//Note this function will iterate elements in current MDSet which is costly.
//Use it carefully.
bool MDSet::is_overlap_ex(MD const* md, Region const* current_rg,
                          MDSystem const* mdsys) const
{
    ASSERT0(md && mdsys && current_rg);
    if (MDSet::is_overlap(md, current_rg)) { return true; }
    MDSetIter iter = nullptr;
    for (BSIdx i = get_first(&iter);
         i != BS_UNDEF; i = get_next(i, &iter)) {
        MD const* t = const_cast<MDSystem*>(mdsys)->getMD((MDIdx)i);
        ASSERT0(t);
        if (t->is_overlap(md)) { return true; }
    }
    return false;
}


void MDSet::bunion(MDSet const& mds, DefMiscBitSetMgr & mbsmgr)
{
    if (this == &mds) { return; }

    ASSERT0(!((DefSBitSetCore&)mds).is_contain(0));

    //TBD: Does it necessary to judge if either current
    //MD or input MD is FULL_MEM?
    //As we observed, passes that utilize MD relationship add
    //MD2 to accroding IR's MDSet, which can keep global variables
    //and MD2 dependence.
    //e.g: g=10, #mustdef=MD10, maydef={MD2, MD10}, g is global variable that
    //           #represented in Program Region.
    //     foo(); #maydef={MD2, MD10}
    //if (DefSBitSetCore::is_contain(MD_FULL_MEM)) {
    //    return;
    //}
    //if (((DefSBitSetCore const&)mds).is_contain(MD_FULL_MEM)) {
    //    clean(mbsmgr);
    //    DefSBitSetCore::bunion(MD_FULL_MEM, mbsmgr);
    //    return;
    //}

    DefSBitSetCore::bunion((DefSBitSetCore&)mds, mbsmgr);
}


//This function will walk through whole current MDSet and differenciate
//overlapped elements.
//Note this function is very costly.
void MDSet::diffAllOverlapped(MDIdx id, DefMiscBitSetMgr & m,
                              MDSystem const* sys)
{
    MDSetIter iter;
    MD const* srcmd = const_cast<MDSystem*>(sys)->getMD(id);
    BSIdx next_i;
    for (BSIdx i = get_first(&iter); i != BS_UNDEF; i = next_i) {
        next_i = get_next(i, &iter);
        MD const* tgtmd = const_cast<MDSystem*>(sys)->getMD(i);
        ASSERT0(tgtmd);
        if (srcmd->is_exact_cover(tgtmd)) {
            diff(i, m);
        }
    }
}


void MDSet::dump(MDSystem * ms, bool detail) const
{
    if (!ms->getRegionMgr()->isLogMgrInit()) { return; }
    ASSERT0(ms);
    MDSetIter iter;
    for (BSIdx i = get_first(&iter); i != BS_UNDEF;) {
        prt(ms->getRegionMgr(), "MD%d", i);
        i = get_next(i, &iter);
        if (i != BS_UNDEF) {
            prt(ms->getRegionMgr(), ",");
        }
    }
    if (detail) {
        for (BSIdx i = get_first(&iter);
             i != BS_UNDEF; i = get_next(i, &iter)) {
            MD const* md = ms->getMD((MDIdx)i);
            ASSERT0(md);
            md->dump(ms->getTypeMgr());
        }
    }
}


MD * MDSet::get_unique_md(MDSystem const* ms) const
{
    MDSetIter it = nullptr;
    BSIdx first = get_first(&it);
    if (first == BS_UNDEF) { return nullptr; }
    BSIdx second = get_next(first, &it);
    return second != BS_UNDEF ? nullptr : ms->getMD((MDIdx)first);
}
//END MDSet


//
//START MDSetHash
//
void MDSetHash::dump(Region * rg)
{
    if (!rg->isLogMgrInit()) { return; }
    note(rg, "\n==---- DUMP MDSet Hash ----==\n");
    SBitSetCoreHash<MDSetHashAllocator>::dump_hashed_set(
        rg->getLogMgr()->getFileHandler());
    SBitSetCoreHash<MDSetHashAllocator>::dump(
        rg->getLogMgr()->getFileHandler());
}
//END MDSetHash


//
//START MDSetMgr
//
//Clean and give it back to md set manager.
//Do not destroy mds.
//Destroy MDSet manager.
MDSetMgr::MDSetMgr(Region * rg, DefMiscBitSetMgr * mbsm)
{
    m_mds_pool = smpoolCreate(sizeof(MDSet) * 8, MEM_CONST_SIZE);
    m_sc_mds_pool = smpoolCreate(sizeof(xcom::SC<MDSet*>) * 8, MEM_CONST_SIZE);
    m_md_set_list.set_pool(m_sc_mds_pool);
    m_free_md_set.set_pool(m_sc_mds_pool);
    m_rg = rg;
    ASSERT0(mbsm);
    m_misc_bs_mgr = mbsm;
}


void MDSetMgr::destroy()
{
    m_free_md_set.clean();
    for (xcom::SC<MDSet*> * sc = m_md_set_list.get_head();
         sc != m_md_set_list.end(); sc = m_md_set_list.get_next(sc)) {
        MDSet * mds = sc->val();
        ASSERT0(mds);
        mds->clean(*m_misc_bs_mgr);
    }
    m_md_set_list.clean();

    smpoolDelete(m_mds_pool);
    smpoolDelete(m_sc_mds_pool);
    m_mds_pool = nullptr;
    m_sc_mds_pool = nullptr;
}


void MDSetMgr::free(MDSet * mds)
{
    if (mds == nullptr) { return; }

    #if 0
    //#ifdef _DEBUG_
    //Caution: this verification is pretty slowly, even if in debug
    //mode, so be patient.

    xcom::SC<MDSet*> * sct;
    for (MDSet * x = m_free_md_set.get_head(&sct);
         x != nullptr; x = m_free_md_set.get_next(&sct)) {
        ASSERTN(x != mds, ("Already have been freed."));
    }
    #endif

    mds->clean(*m_misc_bs_mgr);
    m_free_md_set.append_head(mds);
}


size_t MDSetMgr::count_mem() const
{
    size_t count = 0;
    for (xcom::SC<MDSet*> * sc = m_md_set_list.get_head();
         sc != m_md_set_list.end(); sc = m_md_set_list.get_next(sc)) {
        MDSet const* mds = sc->val();
        ASSERT0(mds);
        count += (UINT)mds->count_mem();
    }
    return count;
}


void MDSetMgr::dump()
{
    if (!m_rg->isLogMgrInit()) { return; }
    size_t count = 0;
    for (xcom::SC<MDSet*> * sc = m_md_set_list.get_head();
         sc != m_md_set_list.end(); sc = m_md_set_list.get_next(sc)) {
        MDSet const* mds = sc->val();
        ASSERT0(mds);
        count += (UINT)mds->count_mem();
    }

    //Dump mem usage into file.
    List<size_t> lst;
    for (xcom::SC<MDSet*> * sc = m_md_set_list.get_head();
         sc != m_md_set_list.end(); sc = m_md_set_list.get_next(sc)) {
        MDSet const* bs = sc->val();
        ASSERT0(bs);
        size_t c = bs->count_mem();
        bool inserted = false;
        if (m_md_set_list.get_elem_count() < 10000) {
            //Inserting sort complexity is quadratic.
            xcom::C<size_t> * ct;
            UINT n = lst.get_elem_count();
            lst.get_head(&ct);
            UINT i;
            for (i = 0; i < n; i++, ct = lst.get_next(ct)) {
                if (c >= ct->val()) {
                    inserted = true;
                    lst.insert_before(c, ct);
                    break;
                }
            }
        }

        if (!inserted) {
            lst.append_head(c);
        }
    }

    size_t v = lst.get_head();

    note(getRegion(),
         "\n==---- DUMP MDSetMgr: total %d MD_SETs, "
         "%d MDSet are in free-list, mem usage are:\n",
         m_md_set_list.get_elem_count(), m_free_md_set.get_elem_count());

    UINT b = 0;
    UINT n = lst.get_elem_count();
    for (UINT i = 0; i < n; i++, v = lst.get_next(), b++) {
        if (b == 20) {
            note(getRegion(), "\n");
            b = 0;
        }

        if (v < 1024) {
            prt(getRegion(), "%luB,", (ULONG)v);
        } else if (v < 1024 * 1024) {
            prt(getRegion(), "%luKB,", (ULONG)v/1024);
        } else {
            prt(getRegion(), "%luMB,", (ULONG)v/1024/1024);
        }
    }
}
//END MDSetMgr


//
//START MD2MD_SET_MAP
//
//Dump all relations between MD, and MDSet.
//'md2mds': mapping from 'md' to an md-set it pointed to.
void MD2MDSet::dump(Region * rg)
{
    StrBuf buf(64);

    if (!rg->isLogMgrInit()) { return; }

    note(rg, "\n==---- DUMP MD2MDSet ----==");

    //Dump all MDs.
    MDSystem * ms = rg->getMDSystem();
    note(rg, "\n==-- DUMP MD Index --==");
    ms->getID2MDMap()->dump(rg);

    MD2MDSetIter mxiter;
    MDSet const* pts = nullptr;
    for (MDIdx mdid = get_first(mxiter, &pts);
         mdid > 0; mdid = get_next(mxiter, &pts)) {
        MD const* md = ms->getMD(mdid);
        ASSERT0(md);

        buf.clean();
        note(rg, "\n\t%s", md->dump(buf, rg->getTypeMgr()));

        //Dumps MDSet related to 'md'.

        ASSERT0(pts);
        note(rg, "\n\t\tPOINT TO:\n");
        MDSetIter iter_j;
        for (BSIdx j = pts->get_first(&iter_j);
             j != BS_UNDEF; j = pts->get_next(j, &iter_j)) {
            MD * mmd = ms->getMD((MDIdx)j);
            ASSERT0(mmd);
            buf.clean();
            prt(rg, "\t\t\t%s\n",
                mmd->dump(buf, rg->getTypeMgr()));
        }
    }

    //Dump set of MD that corresponding to an individual Var.
    note(rg, "\n==-- DUMP the mapping from Var to MDSet --==");
    VarVec * var_tab = rg->getVarMgr()->getVarVec();
    Vector<MD const*> mdv;
    ConstMDIter iter;
    for (VecIdx i = 0; i <= var_tab->get_last_idx(); i++) {
        Var * v = var_tab->get(i);
        if (v == nullptr) { continue; }

        MDTab * mdtab = ms->getMDTab(v);

        buf.clean();
        note(rg, "\n\t%s", v->dump(buf, rg->getTypeMgr()));

        if (mdtab == nullptr || mdtab->get_elem_count() == 0) { continue; }

        mdv.clean();
        iter.clean();
        mdtab->get_elems(mdv, iter);

        for (VecIdx i2 = 0; i2 <= mdv.get_last_idx(); i2++) {
            MD const* md = mdv.get(i2);
            buf.clean();
            note(rg, "\n\t\t%s", md->dump(buf, rg->getTypeMgr()));
        }
    }

    note(rg, "\n");
}
//END MD2MD_SET_MAP


//
//START MDSystem
//
//Register MD and generating unique id for it, with the followed method:
//1. Generating MD hash table for any unique Var.
//2. Entering 'md' into MD hash table, the hash-value comes
//    from an evaluating binary-Tree that the branch of
//    tree-node indicate determination data related with MD fields.
//Return the registered element.
//
//NOTE:
//1. DO NOT free the registered element!
//2. If you want to register an new MD, keep the id is 0.
MD const* MDSystem::registerMD(MD const& m)
{
    ASSERT0(MD_base(&m));
    if (MD_id(&m) > 0) {
        //Find the entry in MDTab accroding to m.
        MDTab * mdtab = getMDTab(MD_base(&m));
        ASSERTN(mdtab != nullptr, ("md has not been registered"));
        MD const* entry = mdtab->find(&m);
        ASSERTN(entry, ("md has not been registered"));
        return entry;
    }

    ASSERT0(MD_base(&m) != nullptr);

    //Check if MD has been registerd.
    MDTab * mdtab = getMDTab(MD_base(&m));
    if (mdtab != nullptr) {
        //Var-base has been registered, then check md by
        //offset in md-table.
        MD const* hash_entry = mdtab->find(&m);
        if (hash_entry != nullptr) {
            //find MD via MD_ofst.
            return hash_entry;
        }

        //TBD: Does it necessary to judge if either current
        //MD or input MD is FULL_MEM?
        //As we observed, passes that utilize MD relationship add
        //MD2 to accroding IR's MDSet, which can keep global variables
        //and MD2 dependence.
        //e.g: g=10, #mustdef=MD10, maydef={MD2, MD10}, g is global variable that
        //           #represented in Program Region.
        //     foo(); #maydef={MD2, MD10}
        //if (MD_base(&m) == m_all_mem) {
        //    return getMD(MD_FULL_MEM);
        //}

        //TODO: remove HEAP, STACK id. I consider they are useless.
        //if (MD_base(md) == g_heap_mem) {
        //    MD_id(md) = MD_HEAP_MEM;
        //    return ::getMD(MD_HEAP_MEM);
        //}
        //if (MD_base(md) == g_stack_mem) {
        //    MD_id(md) = MD_LOCAL_VAR;
        //    return ::getMD(MD_LOCAL_VAR);
        //}
    }

    //Generate a new MD and record it in md-table accroding to its id.
    MD * entry = allocMD();
    if (MD_id(entry) == MD_UNDEF) {
        MD_id(entry) = m_md_count++;
    }
    entry->copy(&m);
    if (mdtab == nullptr) {
        mdtab = allocMDTab();
        m_var2mdtab.set(MD_base(entry), mdtab);
    }

    //Insert entry into MDTab of Var.
    mdtab->append(entry);
    m_id2md_map.set(MD_id(entry), entry);
    return entry;
}


//Register an effectively unbound MD that base is 'var'.
MD const* MDSystem::registerUnboundMD(Var * var, TMWORD size)
{
    MD md;
    MD_base(&md) = var;
    MD_size(&md) = size;
    MD_ty(&md) = MD_UNBOUND;
    return registerMD(md);
}


//MD for local may alias memory.
void MDSystem::initLocalMayAlias(VarMgr * vm)
{
    m_local_may_alias = nullptr;
    if (vm == nullptr) { return; }
    Var * v = vm->registerVar((CHAR const*)".local_may_alias",
                              getTypeMgr()->getMCType(0), 1,
                              VAR_LOCAL|VAR_FAKE|VAR_IS_UNALLOCABLE);
    MD x;
    MD_base(&x) = v;
    MD_size(&x) = 0;
    MD_ty(&x) = MD_UNBOUND;
    MD_is_may(&x) = true; //MD_LOCAL_MAY_ALIAS can only be May reference.
    MD const* e = registerMD(x);
    ASSERT0_DUMMYUSE(e);
    ASSERT0(MD_id(e) == MD_LOCAL_MAY_ALIAS);
    m_local_may_alias = e;
}


//MD for local memory.
void MDSystem::initLocalMem(VarMgr * vm)
{
    m_local_mem = nullptr;
    if (vm == nullptr) { return; }
    Var * v = vm->registerVar((CHAR const*)".local_mem",
                              getTypeMgr()->getMCType(0), 1,
                              VAR_LOCAL|VAR_FAKE|VAR_IS_UNALLOCABLE);
    MD x;
    MD_base(&x) = v;
    MD_size(&x) = 0;
    MD_ty(&x) = MD_UNBOUND;
    MD_is_may(&x) = true; //MD_LOCAL_VAR can only be may-reference.
    MD const* e = registerMD(x);
    ASSERT0_DUMMYUSE(e);
    ASSERT0(MD_id(e) == MD_LOCAL_VAR);
    m_local_mem = e;
}


//MD for HEAP memory.
void MDSystem::initHeapMem(VarMgr * vm)
{
    m_heap_mem = nullptr;
    if (vm == nullptr) { return; }
    Var * v = vm->registerVar((CHAR const*)".heap_mem",
                              getTypeMgr()->getMCType(0), 1,
                              VAR_GLOBAL|VAR_FAKE|VAR_IS_UNALLOCABLE);
    MD x;
    MD_base(&x) = v;
    MD_size(&x) = 0;
    MD_ty(&x) = MD_UNBOUND;
    MD_is_may(&x) = true; //MD_HEAP_MEM can only be May reference.
    MD const* e = registerMD(x);
    ASSERT0_DUMMYUSE(e);
    ASSERT0(MD_id(e) == MD_HEAP_MEM);
    m_heap_mem = e;
}


//MD for global memory.
void MDSystem::initGlobalMem(VarMgr * vm)
{
    m_global_mem = nullptr;
    if (vm == nullptr) { return; }
    Var * v = vm->registerVar((CHAR const*)".global_mem",
                              getTypeMgr()->getMCType(0), 1,
                              VAR_GLOBAL|VAR_FAKE|VAR_IS_UNALLOCABLE);
    MD x;
    MD_base(&x) = v;
    MD_size(&x) = 0;
    MD_ty(&x) = MD_UNBOUND;
    MD_is_may(&x) = true; //MD_GLOBAL_VAR can only be May reference.
    MD const* e = registerMD(x);
    ASSERT0_DUMMYUSE(e);
    ASSERT0(MD_id(e) == MD_GLOBAL_VAR);
    m_global_mem = e;
}


//MD for imported variables.
void MDSystem::initImportMem(VarMgr * vm)
{
    m_import_mem = nullptr;
    if (vm == nullptr) { return; }

    //The design goal of IMPORT MD set is attempt to describe non-global
    //variables precisely that located in outer region.
    //WORKAROUND: In order to speedup analysis, and shrink POINT-TO set size,
    //set IMPORT MD to be GLOBAL, which means GLOBAL variable and IMPORT
    //variable are the same.
    //Primitive design of IMPORT variable is using a fake variable to stand for
    //those local variables that are not located in current region. says outer
    //region.
    //e.g:
    //  Program Region {
    //    Var a; # global variable
    //    Func Region {
    //      Var b; # local
    //      Func Region {
    //        Var c; # local
    //        c = b # b is regarded as the variable that located in
    //              # IMPORT variable set.
    //      }
    //    }
    //  }
    Var * v = vm->registerVar((CHAR const*)".import_var",
                              getTypeMgr()->getMCType(0), 1,
                              VAR_GLOBAL|VAR_FAKE|VAR_IS_UNALLOCABLE);
    MD x;
    MD_base(&x) = v;
    MD_size(&x) = 0;
    MD_ty(&x) = MD_UNBOUND;
    MD_is_may(&x) = true; //MD_IMPORT_VAR can only be May reference.
    MD const* e = registerMD(x);
    ASSERT0_DUMMYUSE(e);
    ASSERT0(MD_id(e) == MD_IMPORT_VAR);
    m_import_mem = e;
}


//MD for total memory.
void MDSystem::initFullMem(VarMgr * vm)
{
    m_full_mem = nullptr;
    if (vm == nullptr) { return; }
    Var * v = vm->registerVar((CHAR const*)".full_mem",
                              getTypeMgr()->getMCType(0),
                              1, VAR_GLOBAL|VAR_FAKE|VAR_IS_UNALLOCABLE);
    MD x;
    MD_base(&x) = v;
    MD_is_may(&x) = true;  //MD_FULL_MEM can only be May reference.
    MD_size(&x) = 0;
    MD_ty(&x) = MD_UNBOUND;
    MD const* e = registerMD(x);
    ASSERT0_DUMMYUSE(e);
    ASSERT0(MD_id(e) == MD_FULL_MEM);
    m_full_mem = e;
}


void MDSystem::initDelegate(VarMgr * vm)
{
    initFullMem(vm);
    initGlobalMem(vm);
    initImportMem(vm);
    initLocalMem(vm);
    initHeapMem(vm);
    initLocalMayAlias(vm);
    ASSERT0(m_md_count == MD_FIRST_ALLOCABLE);
}


void MDSystem::init(VarMgr * vm)
{
    m_pool = smpoolCreate(sizeof(MD) * 5, MEM_CONST_SIZE);
    m_sc_mdptr_pool = smpoolCreate(sizeof(xcom::SC<MD*>) * 10, MEM_CONST_SIZE);
    m_free_md_list.set_pool(m_sc_mdptr_pool);
    m_enable_local_var_delegate = g_enable_local_var_delegate;
    m_md_count = 1;
    m_tm = vm->getTypeMgr();
    ASSERT0(m_tm);
    initDelegate(vm);
}


void MDSystem::destroy()
{
    Var2MDTabIter iter;
    MDTab * mdtab;
    for (Var const* var = m_var2mdtab.get_first(iter, &mdtab);
         var != nullptr; var = m_var2mdtab.get_next(iter, &mdtab)) {
        delete mdtab;
    }
    smpoolDelete(m_pool);
    smpoolDelete(m_sc_mdptr_pool);
}


void MDSystem::addDelegate(Region const* current_rg, MD const* md,
                           OUT MDSet & output, DefMiscBitSetMgr & mbsmgr)
{
    if (md->is_global()) {
        output.bunion_pure(MD_GLOBAL_VAR, mbsmgr);
        return;
    }
    if (isImportVar(md, current_rg)) {
        //'md' indicates local memory but not belong to current region.
        output.bunion_pure(MD_IMPORT_VAR, mbsmgr);
        return;
    }
    ASSERT0(md->is_local());
    if (isEnableLocalVarDelegate()) {
        //'md' indicates local memory to current region.
        output.bunion_pure(MD_LOCAL_VAR, mbsmgr);
    }
    if (md->is_taken_addr()) {
        output.bunion_pure(MD_LOCAL_MAY_ALIAS, mbsmgr);
    }
}


//Compute all other MD which are overlapped with 'md', the output
//will include 'md' itself if there are overlapped MDs.
//e.g: given md1, and md1 overlapped with md2, md3,
//then output set is {md1, md2, md3}.
//md: input to compute the overlapped md-set.
//tabiter: for local use.
//strictly: set to true to compute if md may be overlapped
//            with global variables or import variables.
//Note this function does NOT clean output, and will append result to output.
void MDSystem::computeOverlap(Region * current_rg, MD const* md,
                              OUT MDSet & output, ConstMDIter & tabiter,
                              DefMiscBitSetMgr & mbsmgr, bool strictly)
{
    ASSERT0(md && current_rg);
    if (strictly) {
        addDelegate(current_rg, md, output, mbsmgr);
    }

    MDTab * mdt = getMDTab(MD_base(md));
    ASSERT0(mdt != nullptr);

    MD const* effect_md = mdt->get_effect_md();
    if (effect_md != nullptr && effect_md != md) {
        ASSERT0(MD_base(md) == MD_base(effect_md));
        output.bunion(effect_md, mbsmgr);
    }

    OffsetTab * ofsttab = mdt->get_ofst_tab();
    ASSERT0(ofsttab);
    if (ofsttab->get_elem_count() == 0) { return; }

    tabiter.clean();
    bool find_overlapped = false;
    for (MD const* tmd = ofsttab->get_first(tabiter, nullptr);
         tmd != nullptr; tmd = ofsttab->get_next(tabiter, nullptr)) {
        ASSERT0(MD_base(md) == MD_base(tmd));
        if (tmd == md) { continue; }
        if (md->is_overlap(tmd)) {
            output.bunion(tmd, mbsmgr);
            find_overlapped = true;
        }
    }
    if (find_overlapped) {
        //Add md itself into the output set because the function compute the
        //overlap-set according to 'md', thus record the causality between
        //'md' and its overlap-set.
        output.bunion(md, mbsmgr);
    }
}


//Compute overlapped Exact MD with x, then add result to output.
//Note this function does NOT clean output, and will append result to output.
void MDSystem::computeOverlapExactMD(MD const* md, OUT MDSet * output,
                                     ConstMDIter & mditer,
                                     DefMiscBitSetMgr & mbsmgr)
{
    ASSERT0(md && md->is_exact());
    MDTab * mdt = getMDTab(MD_base(md));
    ASSERT0(mdt);

    OffsetTab * ofstab = mdt->get_ofst_tab();
    ASSERT0(ofstab);
    if (ofstab->get_elem_count() > 0) {
        mditer.clean();
        for (MD const* t = ofstab->get_first(mditer, nullptr);
             t != nullptr; t = ofstab->get_next(mditer, nullptr)) {
            if (t == md || !t->is_exact()) { continue; }
            if (t->is_overlap(md)) {
                output->bunion(t, mbsmgr);
            }
        }
    }
}


//Compute all other MD which are overlapped with MD in set 'mds'.
//e.g: mds contains {md1}, and md1 overlapped with md2, md3,
//then output set 'mds' is {md1, md2, md3}.
//mds: it is not only input but also output buffer.
//added: records the new MD that added into 'mds'.
//mditer: for local use.
//strictly: set to true to compute if md may be overlapped with global memory.
void MDSystem::computeOverlap(Region * current_rg, MOD MDSet & mds,
                              MOD Vector<MD const*> & added,
                              ConstMDIter & mditer,
                              DefMiscBitSetMgr & mbsmgr, bool strictly)
{
    ASSERT0(current_rg);
    UINT count = 0;
    added.clean();
    bool set_global = false;
    bool set_import_var = false;
    MDSetIter iter;
    for (BSIdx i = mds.get_first(&iter);
         i != BS_UNDEF; i = mds.get_next(i, &iter)) {
        MD * md = getMD((MDIdx)i);
        ASSERT0(md);
        MDTab * mdt = getMDTab(MD_base(md));
        ASSERT0(mdt != nullptr);
        if (md->is_global()) {
            set_global = true;
        } else if (!current_rg->isRegionVAR(md->get_base())) {
            set_import_var = true;
        }

        MD const* effect_md = mdt->get_effect_md();
        if (effect_md != nullptr && !mds.is_contain(effect_md, current_rg)) {
            ASSERT0(MD_base(md) == MD_base(effect_md));
            added.set(count, effect_md);
            count++;
        }

        OffsetTab * ofsttab = mdt->get_ofst_tab();
        ASSERT0(ofsttab);
        mditer.clean();
        for (MD const* tmd = ofsttab->get_first(mditer, nullptr);
             tmd != nullptr; tmd = ofsttab->get_next(mditer, nullptr)) {
            if (((DefSBitSetCore&)mds).is_contain(MD_id(tmd))) {
                continue;
            }
            ASSERT0(MD_base(md) == MD_base(tmd));
            if (md->is_overlap(tmd)) {
                added.set(count, tmd);
                count++;
            }
        }
    }

    if (strictly) {
        if (set_global) {
            mds.bunion(getMD(MD_GLOBAL_VAR), mbsmgr);
        }
        if (set_import_var) {
            mds.bunion(getMD(MD_IMPORT_VAR), mbsmgr);
        }
    }

    for (VecIdx i = 0; i <= added.get_last_idx(); i++) {
        MD const* t = added.get(i);
        ASSERT0(t && t->is_effect());
        mds.bunion(t, mbsmgr);
    }
}


MD const* MDSystem::getDelegate(MDIdx mdid) const
{
    switch (mdid) {
    case MD_FULL_MEM: return m_full_mem;
    case MD_GLOBAL_VAR: return m_global_mem;
    case MD_IMPORT_VAR: return m_import_mem;
    case MD_LOCAL_VAR: return m_local_mem;
    case MD_HEAP_MEM: return m_heap_mem;
    case MD_LOCAL_MAY_ALIAS: return m_local_may_alias;
    default: ASSERT0(0);
    }
    return nullptr;
}


MD const* MDSystem::getDelegate(Region const* rg, MD const* md) const
{
    if (md->is_global()) {
        return m_global_mem;
    }
    ASSERT0(md->is_local());
    if (!rg->isRegionVAR(md->get_base())) {
        //'md' indicates local memory but not belong to current region.
        return m_import_mem;
    }
    if (md->is_taken_addr()) {
        return m_local_may_alias;
    }
    //'md' indicates local memory to given region.
    return m_local_mem;
}


bool MDSystem::isImportVar(MD const* md, Region const* rg)
{
    ASSERT0(!md->is_global());
    if (MDSystem::isLocalDelegate(md->id())) { return false; }
    return !rg->isRegionVAR(md->get_base());
}


//Compute all other MD which are overlapped with MD in set 'mds'.
//e.g: mds contains {md1}, and md1 overlapped with md2, md3,
//then output is {md2, md3}.
//mds: it is readonly input.
//output: output MD set.
//mditer: for local use.
//strictly: set to true to compute if MD may be overlapped with delegate.
//Note 'output' do not need to clean before invoke this function.
void MDSystem::computeOverlap(Region * current_rg, MDSet const& mds,
                              OUT MDSet & output, ConstMDIter & mditer,
                              DefMiscBitSetMgr & mbsmgr, bool strictly)
{
    ASSERT0(&mds != &output);
    ASSERT0(current_rg);
    bool has_global = false;
    bool has_import = false;
    bool has_local = false;
    bool has_taken_addr = false;
    List<MD const*> delegates;
    MDSetIter iter;
    for (BSIdx i = mds.get_first(&iter);
         i != BS_UNDEF; i = mds.get_next(i, &iter)) {
        if (isDelegate(i)) { continue; }
        MD * md = getMD((MDIdx)i);
        ASSERT0(md);
        MDTab * mdt = getMDTab(MD_base(md));
        ASSERT0(mdt != nullptr);
    
        //Record the category of variable.
        if (md->is_global()) {
            has_global = true;
        } else if (isImportVar(md, current_rg)) {
            has_import = true;
        } else {
            //Note the md may be local-delegate.
            has_local = true;
            if (md->is_taken_addr()) {
                has_taken_addr = true;
            }
        }

        MD const* effect_md = mdt->get_effect_md();
        if (effect_md != nullptr && !mds.is_contain_pure(MD_id(effect_md))) {
            ASSERT0(MD_base(md) == MD_base(effect_md));
            output.bunion_pure(MD_id(effect_md), mbsmgr);
        }

        OffsetTab const* ofsttab = mdt->get_ofst_tab();
        ASSERT0(ofsttab);
        mditer.clean();
        for (MD const* tmd = ofsttab->get_first(mditer, nullptr);
             tmd != nullptr; tmd = ofsttab->get_next(mditer, nullptr)) {
            if (mds.is_contain_pure(MD_id(tmd))) {
                continue;
            }
            ASSERT0(MD_base(md) == MD_base(tmd));
            if (md->is_overlap(tmd)) {
                output.bunion_pure(MD_id(tmd), mbsmgr);
            }
        }
    }
    if (!strictly) { return; }
    if (has_global) { output.bunion_pure(MD_GLOBAL_VAR, mbsmgr); }
    if (has_import) { output.bunion_pure(MD_IMPORT_VAR, mbsmgr); }
    if (has_local) {
        if (isEnableLocalVarDelegate()) {
            output.bunion_pure(MD_LOCAL_VAR, mbsmgr);
        }
        if (has_taken_addr) {
            output.bunion_pure(MD_LOCAL_MAY_ALIAS, mbsmgr);
        }
    }
}


void MDSystem::clean()
{
    Var2MDTabIter iter;
    MDTab * mdtab;
    for (Var const* var = m_var2mdtab.get_first(iter, &mdtab);
         var != nullptr; var = m_var2mdtab.get_next(iter, &mdtab)) {
        mdtab->clean();
    }
    for (VecIdx i = 0; i <= m_id2md_map.get_last_idx(); i++) {
        MD * md = m_id2md_map.get(i);
        if (md == nullptr) { continue; }
        freeMD(md);
    }
    m_md_count = 2; //Index 0 is reserved, index 1 is all-mem-id.
}


void MDSystem::dump(bool only_dump_nonpr_md)
{
    if (!getRegionMgr()->isLogMgrInit()) { return; }
    if (only_dump_nonpr_md) {
        note(getTypeMgr()->getRegionMgr(), "\n==---- DUMP NON-PR MD ----==");
    } else {
        note(getTypeMgr()->getRegionMgr(), "\n==---- DUMP ALL MD ----==");
    }
    for (VecIdx i = 0; i <= m_id2md_map.get_last_idx(); i++) {
        MD * md = m_id2md_map.get(i);
        if (md == nullptr ||
            (only_dump_nonpr_md && MD_is_pr(md))) {
            continue;
        }
        ASSERT0(MD_id(md) == (MDIdx)i);
        md->dump(getTypeMgr());
    }
}


//Remove all MDs related to specific variable 'v'.
void MDSystem::removeMDforVAR(Var const* v, ConstMDIter & iter)
{
    ASSERT0(v);
    MDTab * mdtab = getMDTab(v);
    if (mdtab != nullptr) {
        MD const* x = mdtab->get_effect_md();
        if (x != nullptr) {
            MD * freeone = getMD(MD_id(x));
            freeMD(freeone);
        }

        OffsetTab * ofstab = mdtab->get_ofst_tab();
        ASSERT0(ofstab);
        if (ofstab->get_elem_count() > 0) {
            iter.clean();
            for (MD const* md = ofstab->get_first(iter, nullptr);
                 md != nullptr; md = ofstab->get_next(iter, nullptr)) {
                MD * freeone = getMD(MD_id(md));
                freeMD(freeone);
            }
        }
        delete mdtab;
    }
    m_var2mdtab.remove(v);
}
//END MDSystem

} //namespace xoc
