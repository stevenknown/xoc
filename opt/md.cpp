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
void MDId2MD::dump() const
{
    if (g_tfile == NULL) { return; }
    for (INT i = 0; i <= get_last_idx(); i++) {
        MD * md = Vector<MD*>::get(i);
        if (md == NULL) { continue; }
        ASSERT0(MD_id(md) == (UINT)i);
        prt("%d,", i);
    }
    fflush(g_tfile);
}
//END MDID


//
//START MD
//
//Check if 'this' cover 'm'.
bool MD::is_cover(MD const* m) const
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


//Check overlapping.
bool MD::is_overlap(MD const* m) const
{
    ASSERT0(m && this != m);
    //TO BE CONFIRMED: Does it necessary to judge if either current
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

    ASSERT0(MD_base(this) != NULL);
    MD_base(this)->dump(buf, dm);

    INT lofst = MD_ofst(this);
    if (MD_ty(this) == MD_EXACT) {
        buf.strcat(" -- ofst:%d -- size:%u", lofst, MD_size(this));
    } else if (MD_ty(this) == MD_RANGE) {
        buf.strcat(" -- start:%d -- end:%u", lofst, lofst + MD_size(this));
        buf.strcat(" -- range");
    } else {
        buf.strcat(" -- ofst:unbound");
    }
    return buf.buf;
}


void MD::dump(TypeMgr * dm) const
{
    if (g_tfile == NULL) { return; }

    StrBuf buf(64);
    note("\n%s", dump(buf, dm));
    fflush(g_tfile);
}
//END MD


//
//START MDSet
//
//Get unique MD that is not fake memory object,
//but its offset might be invalid.
MD * MDSet::get_effect_md(MDSystem * ms) const
{
    ASSERT0(ms);
    if (get_elem_count() != 1) {
        return NULL;
    }
    SEGIter * iter;
    MD * md = ms->getMD(get_first(&iter));
    ASSERT0(md != NULL);
    if (md->get_base()->is_fake()) {
        return NULL;
    }
    return md;
}


void MDSet::bunion(UINT mdid, DefMiscBitSetMgr & mbsmgr)
{
    //TO BE CONFIRMED: Does it necessary to judge if either current
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
bool MDSet::is_exact_equal(MDSet const& mds, MDSystem * ms) const
{
    ASSERT0(ms);
    UINT count = 0;
    INT md1 = -1;
    SEGIter * iter;
    for (INT i = get_first(&iter); i != -1; i = get_next(i, &iter)) {
        if (!ms->getMD(i)->is_exact()) {
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
        if (!ms->getMD(i)->is_exact()) {
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


bool MDSet::is_contain_only_exact_and_str(MDSystem * ms) const
{
    ASSERT0(ms);
    SEGIter * iter;
    for (INT i = get_first(&iter); i != -1; i = get_next(i, &iter)) {
        MD * tmd = ms->getMD(i);
        ASSERT0(tmd != NULL);
        if (!tmd->is_exact() && !MD_base(tmd)->is_string()) {
            return false;
        }
    }
    return true;
}


bool MDSet::is_contain_inexact(MDSystem * ms) const
{
    ASSERT0(ms);
    SEGIter * iter;
    for (INT i = get_first(&iter); i != -1; i = get_next(i, &iter)) {
        MD * tmd = ms->getMD(i);
        ASSERT0(tmd != NULL);

        //TO BE CONFIRMED: Does it necessary to judge if either current
        //MD or input MD is FULL_MEM?
        //As we observed, passes that utilize MD relationship add
        //MD2 to accroding IR's MDSet, which can keep global variables
        //and MD2 dependence.
        //e.g: g=10, #mustdef=MD10, maydef={MD2, MD10}, g is global variable that
        //           #represented in Program Region.
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


//Return true if md is overlap with the elements in set.
bool MDSet::is_overlap(MD const* md, Region * current_ru) const
{
    ASSERT0(current_ru);

    if (md->is_global() &&
        DefSBitSetCore::is_contain(MD_GLOBAL_VAR) &&
        MD_id(md) != MD_IMPORT_VAR) {
        return true;
    }

    //TO BE CONFIRMED: Does it necessary to judge if either current
    //MD or input MD is FULL_MEM?
    //As we observed, passes that utilize MD relationship add
    //MD2 to accroding IR's MDSet, which can keep global variables
    //and MD2 dependence.
    //e.g: g=10, #mustdef=MD10, maydef={MD2, MD10}, g is global variable that
    //           #represented in Program Region.
    //     foo(); #maydef={MD2, MD10}
    //if ((DefSBitSetCore::is_contain(MD_FULL_MEM)) ||
    //    (MD_id(md) == MD_FULL_MEM && !DefSBitSetCore::is_empty())) {
    //    return true;
    //}
 
    if (DefSBitSetCore::is_contain(MD_IMPORT_VAR) &&
        !current_ru->isRegionVAR(md->get_base())) {
        //If current MDSet contains imported variable, it
        //overlaps with IMPORT_VAR.
        return true;
    }
    return DefSBitSetCore::is_contain(MD_id(md));
}


//Return true if 'md' overlapped with element in current MDSet.
//Note this function will iterate elements in current MDSet which is costly.
//Use it carefully.
bool MDSet::is_overlap_ex(MD const* md,
                          Region * current_ru,
                          MDSystem const* mdsys) const
{
    ASSERT0(md && mdsys && current_ru);
    if (MDSet::is_overlap(md, current_ru)) { return true; }

    SEGIter * iter = NULL;
    for (INT i = get_first(&iter);
         i >= 0; i = get_next((UINT)i, &iter)) {
        MD const* t = const_cast<MDSystem*>(mdsys)->getMD((UINT)i);
        ASSERT0(t);
        if (t->is_overlap(md)) { return true; }
    }
    return false;
}


void MDSet::bunion(MDSet const& mds, DefMiscBitSetMgr & mbsmgr)
{
    if (this == &mds) { return; }

    ASSERT0(!((DefSBitSetCore&)mds).is_contain(0));

    //TO BE CONFIRMED: Does it necessary to judge if either current
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


void MDSet::dump(MDSystem * ms, bool detail) const
{
    if (g_tfile == NULL) { return; }
    ASSERT0(ms);

    SEGIter * iter;
    for (INT i = get_first(&iter); i >= 0;) {
        prt("MD%d", i);
        i = get_next(i, &iter);
        if (i >= 0) {
            prt(",");
        }
    }
    if (detail) {
        for (INT i = get_first(&iter); i != -1; i = get_next(i, &iter)) {
            MD const* md = ms->getMD(i);
            ASSERT0(md);
            md->dump(ms->getTypeMgr());
        }
    }
    fflush(g_tfile);
}
//END MDSet


//
//START MDSetHash
//
void MDSetHash::dump()
{
    if (g_tfile == NULL) { return; }
    note("\n==---- DUMP MDSet Hash ----==\n");
    SBitSetCoreHash<MDSetHashAllocator>::dump_hashed_set(g_tfile);
    SBitSetCoreHash<MDSetHashAllocator>::dump(g_tfile);
    fflush(g_tfile);
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
    m_ru = rg;
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
    m_mds_pool = NULL;
    m_sc_mds_pool = NULL;
}


void MDSetMgr::free(MDSet * mds)
{
    if (mds == NULL) { return; }

    #if 0
    //#ifdef _DEBUG_
    //Caution: this verification is pretty slowly, even if in debug
    //mode, so be patient.

    xcom::SC<MDSet*> * sct;
    for (MDSet * x = m_free_md_set.get_head(&sct);
         x != NULL; x = m_free_md_set.get_next(&sct)) {
        ASSERTN(x != mds, ("Already have been freed."));
    }
    #endif

    mds->clean(*m_misc_bs_mgr);
    m_free_md_set.append_head(mds);
}


UINT MDSetMgr::count_mem()
{
    UINT count = 0;
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
    if (g_tfile == NULL) { return; }

    FILE * h = g_tfile;
    UINT count = 0;
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

    fprintf(h,
        "\n==---- DUMP MDSetMgr: total %d MD_SETs, "
        "%d MDSet are in free-list, mem usage are:\n",
        m_md_set_list.get_elem_count(), m_free_md_set.get_elem_count());

    UINT b = 0;
    UINT n = lst.get_elem_count();
    for (UINT i = 0; i < n; i++, v = lst.get_next(), b++) {
        if (b == 20) {
            fprintf(h, "\n");
            b = 0;
        }

        if (v < 1024) {
            fprintf(h, "%luB,", (ULONG)v);
        } else if (v < 1024 * 1024) {
            fprintf(h, "%luKB,", (ULONG)v/1024);
        } else {
            fprintf(h, "%luMB,", (ULONG)v/1024/1024);
        }
    }
    fflush(h);
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

    if (g_tfile == NULL) { return; }

    note("\n==---- DUMP MD2MDSet ----==");

    //Dump all MDs.
    MDSystem * ms = rg->getMDSystem();
    note("\n==-- DUMP MD Index --==");
    ms->getID2MDMap()->dump();

    MD2MDSetIter mxiter;
    MDSet const* pts = NULL;
    for (UINT mdid = get_first(mxiter, &pts);
         mdid > 0; mdid = get_next(mxiter, &pts)) {
        MD const* md = ms->getMD(mdid);
        ASSERT0(md);

        buf.clean();
        note("\n\t%s", md->dump(buf, rg->getTypeMgr()));

        //Dumps MDSet related to 'md'.

        ASSERT0(pts);
        note("\n\t\tPOINT TO:\n");
        SEGIter * iter_j;
        for (INT j = pts->get_first(&iter_j);
             j >= 0; j = pts->get_next(j, &iter_j)) {
            MD * mmd = ms->getMD(j);
            ASSERT0(mmd);
            buf.clean();
            prt("\t\t\t%s\n",
                    mmd->dump(buf, rg->getTypeMgr()));
        }
    }

    //Dump set of MD that corresponding to an individual VAR.
    note("\n==-- DUMP the mapping from VAR to MDSet --==");
    VarVec * var_tab = rg->getVarMgr()->get_var_vec();
    Vector<MD const*> mdv;
    ConstMDIter iter;
    for (INT i = 0; i <= var_tab->get_last_idx(); i++) {
        VAR * v = var_tab->get(i);
        if (v == NULL) { continue; }

        MDTab * mdtab = ms->getMDTab(v);

        buf.clean();
        note("\n\t%s", v->dump(buf, rg->getTypeMgr()));

        if (mdtab == NULL || mdtab->get_elem_count() == 0) { continue; }

        mdv.clean();
        iter.clean();
        mdtab->get_elems(mdv, iter);

        for (INT i2 = 0; i2 <= mdv.get_last_idx(); i2++) {
            MD const* md = mdv.get(i2);
            buf.clean();
            note("\n\t\t%s", md->dump(buf, rg->getTypeMgr()));
        }
    }

    note("\n");
    fflush(g_tfile);
}
//END MD2MD_SET_MAP


//
//START MDSystem
//
//Register MD and generating unique id for it, with the followed method:
//1. Generating MD hash table for any unique VAR.
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
        ASSERTN(mdtab != NULL, ("md has not been registered"));
        MD const* entry = mdtab->find(&m);
        ASSERTN(entry, ("md has not been registered"));
        return entry;
    }

    ASSERT0(MD_base(&m) != NULL);

    //Check if MD has been registerd.
    MDTab * mdtab = getMDTab(MD_base(&m));
    if (mdtab != NULL) {
        //VAR-base has been registered, then check md by
        //offset in md-table.
        MD const* hash_entry = mdtab->find(&m);
        if (hash_entry != NULL) {
            //find MD via MD_ofst.
            return hash_entry;
        }

        //TO BE CONFIRMED: Does it necessary to judge if either current
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
        //    MD_id(md) = MD_STACK_MEM;
        //    return ::getMD(MD_STACK_MEM);
        //}
    }

    //Generate a new MD and record it in md-table accroding to its id.
    MD * entry = allocMD();
    if (MD_id(entry) == 0) {
        MD_id(entry) = m_md_count++;
    }
    entry->copy(&m);
    if (mdtab == NULL) {
        mdtab = allocMDTab();
        m_var2mdtab.set(MD_base(entry), mdtab);
    }

    //Insert entry into MDTab of VAR.
    mdtab->append(entry);
    m_id2md_map.set(MD_id(entry), entry);
    return entry;
}


//Register an effectively unbound MD that base is 'var'.
MD const* MDSystem::registerUnboundMD(VAR * var, UINT size)
{
    MD md;
    MD_base(&md) = var;
    MD_size(&md) = size;
    MD_ty(&md) = MD_UNBOUND;
    return registerMD(md);
}


//MD for global memory.
void MDSystem::initGlobalMemMD(VarMgr * vm)
{
    m_global_mem = NULL;
    if (vm == NULL) { return; }

    m_global_mem = vm->registerVar(
        (CHAR*)".global_mem",
        getTypeMgr()->getMCType(0),
        1, VAR_GLOBAL|VAR_FAKE|VAR_IS_UNALLOCABLE);
    MD x;
    MD_base(&x) = m_global_mem;
    MD_size(&x) = 0;
    MD_ty(&x) = MD_UNBOUND;
    MD_is_may(&x) = true; //MD_GLOBAL_VAR can only be May reference.
    MD const* e = registerMD(x);
    CHECK_DUMMYUSE(e);
    ASSERT0(MD_id(e) == MD_GLOBAL_VAR);
}


//MD for imported variables.
void MDSystem::initImportVar(VarMgr * vm)
{
    m_import_var = NULL;
    if (vm == NULL) { return; }

	//The design goal of IMPORT MD set is attempt to describe non-global variables
	//precisely that located in outer region.
	//WORKAROUND: In order to speedup analysis, and shrink POINT-TO set size, set
	//IMPORT MD to be GLOBAL, which means GLOBAL variable and IMPORT variable are
	//the same.
	//Primitive design of IMPORT variable is using a fake variable to stand for
	//those local variables that are not located in current region. says outer
	//region.
	//e.g:
	//  Program Region {
	//    VAR a; # global variable
	//    Func Region {
	//      VAR b; # local
	//      Func Region {
	//        VAR c; # local
	//        # b is regarded as the variable that located in IMPORT variable set.
	//      }
    //    }
    //  }
    m_import_var = vm->registerVar(
                        (CHAR*)".import_var",
                        getTypeMgr()->getMCType(0),
                        1, VAR_GLOBAL|VAR_FAKE|VAR_IS_UNALLOCABLE);
    MD x;
    MD_base(&x) = m_import_var;
    MD_size(&x) = 0;
    MD_ty(&x) = MD_UNBOUND;
    MD_is_may(&x) = true; //MD_IMPORT_VAR can only be May reference.
    MD const* e = registerMD(x);
    CHECK_DUMMYUSE(e);
    ASSERT0(MD_id(e) == MD_IMPORT_VAR);
}




//MD for total memory.
void MDSystem::initAllMemMD(VarMgr * vm)
{
    m_all_mem = NULL;
    if (vm == NULL) { return; }

    m_all_mem = vm->registerVar(
                    (CHAR*)".all_mem",
                    getTypeMgr()->getMCType(0),
                    1,
                    VAR_GLOBAL|VAR_FAKE|VAR_IS_UNALLOCABLE);
    MD x;
    MD_base(&x) = m_all_mem;
    MD_is_may(&x) = true;  //MD_FULL_MEM can only be May reference.
    MD_size(&x) = 0;
    MD_ty(&x) = MD_UNBOUND;
    MD const* e = registerMD(x);
    CHECK_DUMMYUSE(e);
    ASSERT0(MD_id(e) == MD_FULL_MEM);
}


void MDSystem::init(VarMgr * vm)
{
    m_pool = smpoolCreate(sizeof(MD) * 5, MEM_CONST_SIZE);
    m_sc_mdptr_pool = smpoolCreate(sizeof(xcom::SC<MD*>) * 10, MEM_CONST_SIZE);
    m_free_md_list.set_pool(m_sc_mdptr_pool);
    m_md_count = 1;
    m_tm = vm->getTypeMgr();
    ASSERT0(m_tm);
    initAllMemMD(vm);
    initGlobalMemMD(vm);
    initImportVar(vm);
    ASSERT0(m_md_count == MD_FIRST_ALLOCABLE);
}


void MDSystem::destroy()
{
    TMapIter<VAR const*, MDTab*> iter;
    MDTab * mdtab;
    for (VAR const* var = m_var2mdtab.get_first(iter, &mdtab);
         var != NULL; var = m_var2mdtab.get_next(iter, &mdtab)) {
        delete mdtab;
    }

    smpoolDelete(m_pool);
    smpoolDelete(m_sc_mdptr_pool);
}


//Compute all other md which are overlapped with 'md', but the output
//does not include md itself.
//e.g: md is md1, and md1 overlapped with md2, md3,
//then output set is {md2, md3}.
//
//'md': input to compute the overlapped md-set.
//'tmpvec': for local use.
//'tabiter': for local use.
//'strictly': set to true to compute if md may be overlapped with global memory.
//
//Note this function does NOT clean output, and will append result to output.
void MDSystem::computeOverlap(
        Region * current_ru,
        MD const* md,
        MDSet & output,
        ConstMDIter & tabiter,
        DefMiscBitSetMgr & mbsmgr,
        bool strictly)
{
    ASSERT0(md && current_ru);
    if (strictly) {
        if (md->is_global()) {
            output.bunion(MD_GLOBAL_VAR, mbsmgr);
        } else if (!current_ru->isRegionVAR(md->get_base())) {
            output.bunion(MD_IMPORT_VAR, mbsmgr);
        }
    }

    MDTab * mdt = getMDTab(MD_base(md));
    ASSERT0(mdt != NULL);

    MD const* effect_md = mdt->get_effect_md();
    if (effect_md != NULL && effect_md != md) {
        ASSERT0(MD_base(md) == MD_base(effect_md));
        output.bunion(effect_md, mbsmgr);
    }

    OffsetTab * ofsttab = mdt->get_ofst_tab();
    ASSERT0(ofsttab);
    if (ofsttab->get_elem_count() > 0) {
        tabiter.clean();
        bool find_overlapped = false;
        for (MD const* tmd = ofsttab->get_first(tabiter, NULL);
             tmd != NULL; tmd = ofsttab->get_next(tabiter, NULL)) {
            ASSERT0(MD_base(md) == MD_base(tmd));
            if (tmd == md) { continue; }
            if (md->is_overlap(tmd)) {
                output.bunion(tmd, mbsmgr);
                find_overlapped = true;
            }
        }

        if (find_overlapped) {
            output.bunion(md, mbsmgr);
        }
    }
}


//Compute overlapped Exact MD with x, then add result to output.
//Note this function does NOT clean output, and will append result to output.
void MDSystem::computeOverlapExactMD(
        MD const* md,
        OUT MDSet * output,
        ConstMDIter & tabiter,
        DefMiscBitSetMgr & mbsmgr)
{
    ASSERT0(md && md->is_exact());
    MDTab * mdt = getMDTab(MD_base(md));
    ASSERT0(mdt);

    OffsetTab * ofstab = mdt->get_ofst_tab();
    ASSERT0(ofstab);
    if (ofstab->get_elem_count() > 0) {
        tabiter.clean();
        for (MD const* t = ofstab->get_first(tabiter, NULL);
             t != NULL; t = ofstab->get_next(tabiter, NULL)) {
            if (t == md || !t->is_exact()) { continue; }
            if (t->is_overlap(md)) {
                output->bunion(t, mbsmgr);
            }
        }
    }
}


//Compute all other md which are overlapped with MD in set 'mds'.
//e.g: mds contains {md1}, and md1 overlapped with md2, md3,
//then output set is {md1, md2, md3}.
//
//'mds': it is not only input but also output buffer.
//'tmpvec': for local use.
//'tabiter': for local use.
//'strictly': set to true to compute if md may be overlapped with global memory.
void MDSystem::computeOverlap(
        Region * current_ru,
        IN OUT MDSet & mds,
        Vector<MD const*> & tmpvec,
        ConstMDIter & tabiter,
        DefMiscBitSetMgr & mbsmgr,
        bool strictly)
{
    ASSERT0(current_ru);
    UINT count = 0;
    tmpvec.clean();
    bool set_global = false;
    bool set_import_var = false;
    SEGIter * iter;
    for (INT i = mds.get_first(&iter);
         i >= 0; i = mds.get_next(i, &iter)) {
        MD * md = getMD(i);
        ASSERT0(md);
        MDTab * mdt = getMDTab(MD_base(md));
        ASSERT0(mdt != NULL);
        if (md->is_global()) {
            set_global = true;
        } else if (!current_ru->isRegionVAR(md->get_base())) {
            set_import_var = true;
        }

        MD const* effect_md = mdt->get_effect_md();
        if (effect_md != NULL && !mds.is_contain(effect_md)) {
            ASSERT0(MD_base(md) == MD_base(effect_md));
            tmpvec.set(count, effect_md);
            count++;
        }

        OffsetTab * ofsttab = mdt->get_ofst_tab();
        ASSERT0(ofsttab);
        tabiter.clean();
        for (MD const* tmd = ofsttab->get_first(tabiter, NULL);
             tmd != NULL; tmd = ofsttab->get_next(tabiter, NULL)) {
            if (((DefSBitSetCore&)mds).is_contain(MD_id(tmd))) {
                continue;
            }
            ASSERT0(MD_base(md) == MD_base(tmd));
            if (md->is_overlap(tmd)) {
                tmpvec.set(count, tmd);
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

    for (INT i = 0; i <= tmpvec.get_last_idx(); i++) {
        MD const* t = tmpvec.get(i);
        ASSERT0(t && t->is_effect());
        mds.bunion(t, mbsmgr);
    }
}


//Compute all other md which are overlapped with MD in set 'mds'.
//e.g: mds contains {md1}, and md1 overlapped with md2, md3,
//then output is {md2, md3}.
//
//'mds': it is not only input but also output buffer.
//'output': output md set.
//'tabiter': for local use.
//'strictly': set to true to compute if md may be overlapped with global memory.
//
//Note output do not need to clean before invoke this function.
void MDSystem::computeOverlap(
        Region * current_ru,
        MDSet const& mds,
        OUT MDSet & output,
        ConstMDIter & tabiter,
        DefMiscBitSetMgr & mbsmgr,
        bool strictly)
{
    ASSERT0(&mds != &output);
    ASSERT0(current_ru);

    bool set_global = false;
    bool set_import_var = false;
    SEGIter * iter;
    for (INT i = mds.get_first(&iter); i >= 0; i = mds.get_next(i, &iter)) {
        MD * md = getMD(i);
        ASSERT0(md);
        MDTab * mdt = getMDTab(MD_base(md));
        ASSERT0(mdt != NULL);
        if (md->is_global()) {
            set_global = true;
        } else if (!current_ru->isRegionVAR(md->get_base())) {
            set_import_var = true;
        }

        MD const* effect_md = mdt->get_effect_md();
        if (effect_md != NULL && !mds.is_contain_pure(MD_id(effect_md))) {
            ASSERT0(MD_base(md) == MD_base(effect_md));
            output.bunion_pure(MD_id(effect_md), mbsmgr);
        }

        OffsetTab const* ofsttab = mdt->get_ofst_tab();
        ASSERT0(ofsttab);
        tabiter.clean();
        for (MD const* tmd = ofsttab->get_first(tabiter, NULL);
             tmd != NULL; tmd = ofsttab->get_next(tabiter, NULL)) {
            if (mds.is_contain_pure(MD_id(tmd))) {
                continue;
            }
            ASSERT0(MD_base(md) == MD_base(tmd));
            if (md->is_overlap(tmd)) {
                output.bunion_pure(MD_id(tmd), mbsmgr);
            }
        }
    }

    if (strictly) {
        if (set_global) {
            output.bunion_pure(MD_GLOBAL_VAR, mbsmgr);
        }
        if (set_import_var) {
            output.bunion_pure(MD_IMPORT_VAR, mbsmgr);
        }
    }
}


void MDSystem::clean()
{
    TMapIter<VAR const*, MDTab*> iter;
    MDTab * mdtab;
    for (VAR const* var = m_var2mdtab.get_first(iter, &mdtab);
         var != NULL; var = m_var2mdtab.get_next(iter, &mdtab)) {
        mdtab->clean();
    }

    for (INT i = 0; i <= m_id2md_map.get_last_idx(); i++) {
        MD * md = m_id2md_map.get(i);
        if (md == NULL) { continue; }
        freeMD(md);
    }

    m_md_count = 2; //Index 0 is reserved, index 1 is all-mem-id.
}


void MDSystem::dump(bool only_dump_nonpr_md)
{
    if (g_tfile == NULL) { return; }
    if (only_dump_nonpr_md) {
        note("\n==---- DUMP NON-PR MD ----==");
    } else {
        note("\n==---- DUMP ALL MD ----==");
    }
    for (INT i = 0; i <= m_id2md_map.get_last_idx(); i++) {
        MD * md = m_id2md_map.get(i);
        if (md == NULL ||
            (only_dump_nonpr_md && MD_is_pr(md))) {
            continue;
        }
        ASSERT0(MD_id(md) == (UINT)i);
        md->dump(getTypeMgr());
        fflush(g_tfile);
    }
}


//Remove all MDs related to specific variable 'v'.
void MDSystem::removeMDforVAR(VAR const* v, ConstMDIter & iter)
{
    ASSERT0(v);
    MDTab * mdtab = getMDTab(v);
    if (mdtab != NULL) {
        MD const* x = mdtab->get_effect_md();
        if (x != NULL) {
            MD * freeone = getMD(MD_id(x));
            freeMD(freeone);
        }

        OffsetTab * ofstab = mdtab->get_ofst_tab();
        ASSERT0(ofstab);
        if (ofstab->get_elem_count() > 0) {
            iter.clean();
            for (MD const* md = ofstab->get_first(iter, NULL);
                 md != NULL; md = ofstab->get_next(iter, NULL)) {
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
