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
        fprintf(g_tfile, "%d,", i);
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
    if (MD_base(this) != MD_base(m)) {
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
    if (MD_id(m) == MD_ALL_MEM || MD_id(this) == MD_ALL_MEM) {
        return true;
    }

    //if (MD_id(m) == MD_HEAP_MEM && MD_id(this) == MD_ALL_MEM)
    //{    return true; }

    if (MD_base(m) != MD_base(this)) { return false; }

    if (MD_ty(m) == MD_UNBOUND || MD_ty(this) == MD_UNBOUND) {
        return true;
    }

    return !(((MD_ofst(m) + MD_size(m)) <= MD_ofst(this)) ||
             ((MD_ofst(this) + MD_size(this)) <= MD_ofst(m)));
}


//Caller should make sure 'buf' is large enough.
CHAR * MD::dump(CHAR * buf, UINT bufl, TypeMgr * dm) const
{
    UNUSED(bufl);
    ASSERT0(buf);
    CHAR * tb = buf;
    sprintf(buf, "MD%d -- base:", MD_id(this));
    buf += strlen(buf);
    ASSERT0(MD_base(this) != NULL);
    MD_base(this)->dump(buf, dm);
    buf += strlen(buf);
    INT ofst = MD_ofst(this);
    if (MD_ty(this) == MD_EXACT) {
        sprintf(buf, " -- ofst:%d -- size:%u", ofst, MD_size(this));
    } else if (MD_ty(this) == MD_RANGE) {
        sprintf(buf, " -- start:%d -- end:%u", ofst, ofst + MD_size(this));
        strcat(buf, " -- range");
    } else {
        sprintf(buf, " -- ofst:unbound");
    }
    ASSERT(strlen(tb) < bufl, ("dump buf overflow!"));
    return tb;
}


void MD::dump(TypeMgr * dm) const
{
    if (g_tfile == NULL) return;
    //CHAR buf[MAX_BUF_LEN];
    CHAR buf[4096];
    buf[0] = 0;
    fprintf(g_tfile, "\n%s", dump(buf, 4096, dm));
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
    MD * md = ms->get_md(get_first(&iter));
    ASSERT0(md != NULL);
    if (VAR_is_fake(MD_base(md))) {
        return NULL;
    }
    return md;
}


void MDSet::bunion(UINT mdid, DefMiscBitSetMgr & mbsmgr)
{
    if (mdid == MD_ALL_MEM) {
        clean(mbsmgr);
        DefSBitSetCore::bunion(MD_ALL_MEM, mbsmgr);
        return;
    }
    if (DefSBitSetCore::is_contain(MD_ALL_MEM)) {
        ASSERT0(DefSBitSetCore::get_elem_count() == 1);
        return;
    }
    DefSBitSetCore::bunion(mdid, mbsmgr);
}


void MDSet::bunion(MD const* md, DefMiscBitSetMgr & mbsmgr)
{
    ASSERT0(md && (MD_id(md) != 0));
    if (MD_id(md) == MD_ALL_MEM) {
        clean(mbsmgr);
        DefSBitSetCore::bunion(MD_ALL_MEM, mbsmgr);
        return;
    }
    if (DefSBitSetCore::is_contain(MD_ALL_MEM)) {
        ASSERT0(DefSBitSetCore::get_elem_count() == 1);
        return;
    }
    DefSBitSetCore::bunion(MD_id(md), mbsmgr);
}


//Return true current set is equivalent to mds, and every element
//in set is exact.
bool MDSet::is_exact_equal(IN MDSet const& mds, MDSystem * ms) const
{
    ASSERT0(ms);
    UINT count = 0;
    INT md1 = -1;
    SEGIter * iter;
    for (INT i = get_first(&iter); i != -1; i = get_next(i, &iter)) {
        if (!ms->get_md(i)->is_exact()) {
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
        if (!ms->get_md(i)->is_exact()) {
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
        MD * tmd = ms->get_md(i);
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
        MD * tmd = ms->get_md(i);
        ASSERT0(tmd != NULL);
        if (MD_id(tmd) == MD_ALL_MEM) {
            return true;
        }
        if (!tmd->is_exact()) {
            return true;
        }
    }
    return false;
}


void MDSet::bunion(MDSet const& mds, DefMiscBitSetMgr & mbsmgr)
{
    if (this == &mds) { return; }

    ASSERT0(!((DefSBitSetCore&)mds).is_contain(0));

    if (DefSBitSetCore::is_contain(MD_ALL_MEM)) {
        return;
    }

    if (((DefSBitSetCore const&)mds).is_contain(MD_ALL_MEM)) {
        clean(mbsmgr);
        DefSBitSetCore::bunion(MD_ALL_MEM, mbsmgr);
        return;
    }
    DefSBitSetCore::bunion((DefSBitSetCore&)mds, mbsmgr);
}


void MDSet::dump(MDSystem * ms, bool detail) const
{
    if (g_tfile == NULL) return;
    ASSERT0(ms);

    SEGIter * iter;
    for (INT i = get_first(&iter); i >= 0;) {
        fprintf(g_tfile, "MD%d", i);
        i = get_next(i, &iter);
        if (i >= 0) {
            fprintf(g_tfile, ",");
        }
    }
    if (detail) {
        SEGIter * iter;
        for (INT i = get_first(&iter); i != -1; i = get_next(i, &iter)) {
            MD const* md = ms->get_md(i);
            md->dump(ms->get_type_mgr());
        }
    }
    fflush(g_tfile);
}
//END MDSet


//
//START MDSetMgr
//
//Clean and give it back to md set manager.
//Do not destroy mds.
//Destroy MDSet manager.
MDSetMgr::MDSetMgr(Region * ru, DefMiscBitSetMgr * mbsm)
{
    m_mds_pool = smpoolCreate(sizeof(MDSet) * 8, MEM_CONST_SIZE);
    m_sc_mds_pool = smpoolCreate(sizeof(SC<MDSet*>) * 8, MEM_CONST_SIZE);
    m_md_set_list.set_pool(m_sc_mds_pool);
    m_free_md_set.set_pool(m_sc_mds_pool);
    m_ru = ru;
    ASSERT0(mbsm);
    m_misc_bs_mgr = mbsm;
}


void MDSetMgr::destroy()
{
    m_free_md_set.clean();
    for (SC<MDSet*> * sc = m_md_set_list.get_head();
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

    SC<MDSet*> * sct;
    for (MDSet * x = m_free_md_set.get_head(&sct);
         x != NULL; x = m_free_md_set.get_next(&sct)) {
        ASSERT(x != mds, ("Already have been freed."));
    }
    #endif

    mds->clean(*m_misc_bs_mgr);
    m_free_md_set.append_head(mds);
}


UINT MDSetMgr::count_mem()
{
    UINT count = 0;
    for (SC<MDSet*> * sc = m_md_set_list.get_head();
         sc != m_md_set_list.end(); sc = m_md_set_list.get_next(sc)) {
        MDSet const* mds = sc->val();
        ASSERT0(mds);
        count += mds->count_mem();
    }
    return count;
}


void MDSetMgr::dump()
{
    if (g_tfile == NULL) { return; }

    FILE * h = g_tfile;
    UINT count = 0;
    for (SC<MDSet*> * sc = m_md_set_list.get_head();
         sc != m_md_set_list.end(); sc = m_md_set_list.get_next(sc)) {
        MDSet const* mds = sc->val();
        ASSERT0(mds);
        count += mds->count_mem();
    }

    //Dump mem usage into file.
    List<UINT> lst;
    for (SC<MDSet*> * sc = m_md_set_list.get_head();
         sc != m_md_set_list.end(); sc = m_md_set_list.get_next(sc)) {
        MDSet const* bs = sc->val();
        ASSERT0(bs);
        UINT c = bs->count_mem();

        bool inserted = false;

        if (m_md_set_list.get_elem_count() < 10000) {
            //Inserting sort complexity is quadratic.
            C<UINT> * ct;
            UINT n = lst.get_elem_count();
            lst.get_head(&ct);
            UINT i;
            for (i = 0; i < n; i++, ct = lst.get_next(ct)) {
                if (c >= C_val(ct)) {
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

    UINT v = lst.get_head();

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
            fprintf(h, "%dB,", v);
        } else if (v < 1024 * 1024) {
            fprintf(h, "%dKB,", v/1024);
        } else {
            fprintf(h, "%dMB,", v/1024/1024);
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
void MD2MDSet::dump(Region * ru)
{
    CHAR buf[MAX_BUF_LEN];
    if (g_tfile == NULL) { return; }

    fprintf(g_tfile, "\n*** Dump MD POINT-TO list ***");

    //Dump all MDs.
    MDSystem * ms = ru->get_md_sys();
    ms->get_id2md_map()->dump();
    MD2MDSetIter mxiter;
    MDSet const* pts = NULL;
    for (UINT mdid = get_first(mxiter, &pts);
         mdid > 0; mdid = get_next(mxiter, &pts)) {
        MD const* md = ms->get_md(mdid);
        ASSERT0(md);

        buf[0] = 0;
        fprintf(g_tfile, "\n\t%s", md->dump(buf, MAX_BUF_LEN, ru->get_type_mgr()));

        //Dumps MDSet related to 'md'.

        ASSERT0(pts);
        fprintf(g_tfile, "\n\t\tPOINT TO:\n");
        SEGIter * iter_j;
        for (INT j = pts->get_first(&iter_j);
             j >= 0; j = pts->get_next(j, &iter_j)) {
            MD * mmd = ms->get_md(j);
            ASSERT0(mmd);
            buf[0] = 0;
            fprintf(g_tfile, "\t\t\t%s\n",
                    mmd->dump(buf, MAX_BUF_LEN, ru->get_type_mgr()));
        }
    }

    //Dump set of MD that corresponding to an individual VAR.
    fprintf(g_tfile, "\n*** Dump the mapping from VAR to set of MD ***");
    ID2VAR * var_tab = ru->get_var_mgr()->get_var_vec();
    Vector<MD const*> mdv;
    ConstMDIter iter;
    for (INT i = 0; i <= var_tab->get_last_idx(); i++) {
        VAR * v = var_tab->get(i);
        if (v == NULL) { continue; }

        MDTab * mdtab = ms->get_md_tab(v);

        buf[0] = 0;

        fprintf(g_tfile, "\n\t%s", v->dump(buf, ru->get_type_mgr()));

        if (mdtab && mdtab->get_elem_count() > 0) {
            mdv.clean();
            iter.clean();
            mdtab->get_elems(mdv, iter);

            for (INT i = 0; i <= mdv.get_last_idx(); i++) {
                MD const* md = mdv.get(i);
                buf[0] = 0;
                fprintf(g_tfile, "\n\t\t%s",
                        md->dump(buf, MAX_BUF_LEN, ru->get_type_mgr()));
            }
        } //end if
    } //end for

    fprintf(g_tfile, "\n");
    fflush(g_tfile);
}
//END MD2MD_SET_MAP


//
//START MDSystem
//
/* Register MD and generating unique id for it, with the followed method:
1. Generating MD hash table for any unique VAR.
2. Entering 'md' into MD hash table, the hash-value comes
    from an evaluating binary-Tree that the branch of
    tree-node indicate determination data related with MD fields.
Return the registered element.

NOTICE:
1. DO NOT free the registered element!
2. If you want to register an new MD, keep the id is 0. */
MD const* MDSystem::registerMD(MD const& m)
{
    ASSERT0(MD_base(&m));
    if (MD_id(&m) > 0) {
        //Find the entry accroding to m.
        MDTab * mdtab = get_md_tab(MD_base(&m));
        ASSERT(mdtab != NULL, ("md has registered"));
        MD const* entry = mdtab->find(&m);
        ASSERT(entry, ("md has registered"));
        return entry;
    }

    ASSERT0(MD_base(&m) != NULL);

    //Check if MD has been registerd.
    MDTab * mdtab = get_md_tab(MD_base(&m));
    if (mdtab != NULL) {
        //VAR-base has been registered, then check md by
        //offset in md-table.
        MD const* hash_entry = mdtab->find(&m);
        if (hash_entry != NULL) {
            //find MD via MD_ofst.
            return hash_entry;
        }

        if (MD_base(&m) == m_all_mem) {
            return get_md(MD_ALL_MEM);
        }

        //TODO: remove HEAP, STACK id. I consider they are useless.
        //if (MD_base(md) == g_heap_mem) {
        //    MD_id(md) = MD_HEAP_MEM;
        //    return ::get_md(MD_HEAP_MEM);
        //}
        //if (MD_base(md) == g_stack_mem) {
        //    MD_id(md) = MD_STACK_MEM;
        //    return ::get_md(MD_STACK_MEM);
        //}
    }

    //Generate a new MD and record it in md-table accroding to its id.
    MD * entry = newMD();
    UINT id = m_free_mdid_list.remove_head();
    if (id != 0) {
        MD_id(entry) = id;
    } else {
        MD_id(entry) = m_md_count++;
    }
    entry->copy(&m);
    if (mdtab == NULL) {
        mdtab = newMDTab();

        //MDTab indexed by VAR's id.
        m_var2mdtab.set(MD_base(entry), mdtab);
    }

    //Insert into MD-table.
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
                        get_type_mgr()->getMCType(0),
                        1, VAR_GLOBAL|VAR_FAKE);
    VAR_allocable(m_global_mem) = false;

    MD x;
    //MD_id(&x) = MD_GLOBAL_MEM;
    MD_base(&x) = m_global_mem;
    MD_size(&x) = 0;
    MD_ty(&x) = MD_UNBOUND;
    MD_is_may(&x) = true; //MD_GLOBAL_MEM can only be May reference.
    MD const* e = registerMD(x);
    CK_USE(e);
    ASSERT0(MD_id(e) == MD_GLOBAL_MEM);
}


//MD for total memory.
void MDSystem::initAllMemMD(VarMgr * vm)
{
    m_all_mem = NULL;
    if (vm == NULL) { return; }

    m_all_mem = vm->registerVar(
                    (CHAR*)".all_mem",
                    get_type_mgr()->getMCType(0),
                    1,
                    VAR_GLOBAL|VAR_FAKE);
    VAR_allocable(m_all_mem) = false;

    MD x;
    //MD_id(&x) = MD_ALL_MEM;
    MD_base(&x) = m_all_mem;
    MD_is_may(&x) = true;  //MD_ALL_MEM can only be May reference.
    MD_size(&x) = 0;
    MD_ty(&x) = MD_UNBOUND;
    MD const* e = registerMD(x);
    CK_USE(e);
    ASSERT0(MD_id(e) == MD_ALL_MEM);
}


void MDSystem::init(VarMgr * vm)
{
    m_pool = smpoolCreate(sizeof(MD) * 5, MEM_CONST_SIZE);
    m_sc_mdptr_pool = smpoolCreate(sizeof(SC<MD*>) * 10,
                                           MEM_CONST_SIZE);
    m_sc_mdid_pool = smpoolCreate(sizeof(SC<UINT>) * 10,
                                          MEM_CONST_SIZE);
    m_free_md_list.set_pool(m_sc_mdptr_pool);
    m_free_mdid_list.set_pool(m_sc_mdid_pool);
    m_md_count = 1;
    m_dm = vm->get_type_mgr();
    ASSERT0(m_dm);
    initAllMemMD(vm);
    initGlobalMemMD(vm);
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
    smpoolDelete(m_sc_mdid_pool);
}


/* Compute all other md which are overlapped with 'md', but the output
does not include md itself.
e.g: md is md1, and md1 overlapped with md2, md3,
then output set is {md2, md3}.

'md': input to compute the overlapped md-set.
'tmpvec': for local use.
'tabiter': for local use.
'strictly': set to true to compute if md may be overlapped with global memory.

Note output need to be clean before invoke this function. */
void MDSystem::computeOverlap(MD const* md, MDSet & output,
                             ConstMDIter & tabiter,
                             DefMiscBitSetMgr & mbsmgr,
                             bool strictly)
{
    ASSERT0(md);
    ASSERT0(MD_id(md) != MD_ALL_MEM);

    if (strictly && md->is_global()) {
        output.bunion(get_md(MD_GLOBAL_MEM), mbsmgr);
    }

    MDTab * mdt = get_md_tab(MD_base(md));
    ASSERT0(mdt != NULL);

    MD const* effect_md = mdt->get_effect_md();
    if (effect_md != NULL && effect_md != md) {
        ASSERT0(MD_base(md) == MD_base(effect_md));
        output.bunion(effect_md, mbsmgr);
    }

    OffsetTab * ofsttab = mdt->get_ofst_tab();
    ASSERT0(ofsttab);
    tabiter.clean();
    for (MD const* tmd = ofsttab->get_first(tabiter, NULL);
         tmd != NULL; tmd = ofsttab->get_next(tabiter, NULL)) {
        ASSERT0(MD_base(md) == MD_base(tmd));
        if (tmd == md) { continue; }
        if (md->is_overlap(tmd)) {
            output.bunion(tmd, mbsmgr);
        }
    }
}


/* Compute all other md which are overlapped with MD in set 'mds'.
e.g: mds contains {md1}, and md1 overlapped with md2, md3,
then output set is {md1, md2, md3}.

'mds': it is not only input but also output buffer.
'tmpvec': for local use.
'tabiter': for local use.
'strictly': set to true to compute if md may be overlapped with global memory.
*/
void MDSystem::computeOverlap(IN OUT MDSet & mds,
                            Vector<MD const*> & tmpvec,
                            ConstMDIter & tabiter,
                            DefMiscBitSetMgr & mbsmgr,
                            bool strictly)
{
    if (((DefSBitSetCore&)mds).is_contain(MD_ALL_MEM)) { return; }

    UINT count = 0;
    tmpvec.clean();
    bool set_global = false;
    SEGIter * iter;
    for (INT i = mds.get_first(&iter);
         i >= 0; i = mds.get_next(i, &iter)) {
        MD * md = get_md(i);
        ASSERT0(md);
        MDTab * mdt = get_md_tab(MD_base(md));
        ASSERT0(mdt != NULL);
        if (md->is_global()) { set_global = true; }

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

    if (strictly && set_global) { mds.bunion(get_md(MD_GLOBAL_MEM), mbsmgr); }

    for (INT i = 0; i <= tmpvec.get_last_idx(); i++) {
        MD const* t = tmpvec.get(i);
        ASSERT0(t && t->is_effect());
        mds.bunion(t, mbsmgr);
    }
}


/* Compute all other md which are overlapped with MD in set 'mds'.
e.g: mds contains {md1}, and md1 overlapped with md2, md3,
then output is {md1, md2, md3}.

'mds': it is not only input but also output buffer.
'output': output md set.
'tabiter': for local use.
'strictly': set to true to compute if md may be overlapped with global memory.

Note output do not need to clean before invoke this function. */
void MDSystem::computeOverlap(MDSet const& mds,
                            OUT MDSet & output,
                            ConstMDIter & tabiter,
                            DefMiscBitSetMgr & mbsmgr,
                            bool strictly)
{
    ASSERT0(&mds != &output);

    if (mds.is_contain_pure(MD_ALL_MEM)) { return; }

    bool set_global = false;
    SEGIter * iter;
    for (INT i = mds.get_first(&iter); i >= 0; i = mds.get_next(i, &iter)) {
        MD * md = get_md(i);
        ASSERT0(md);
        MDTab * mdt = get_md_tab(MD_base(md));
        ASSERT0(mdt != NULL);
        if (md->is_global()) { set_global = true; }

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

    if (strictly && set_global) { output.bunion_pure(MD_GLOBAL_MEM, mbsmgr); }
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
    m_free_mdid_list.clean();
}


void MDSystem::dumpAllMD()
{
    if (g_tfile == NULL) return;
    fprintf(g_tfile, "\n==---- DUMP ALL MD ----==");
    for (INT i = 0; i <= m_id2md_map.get_last_idx(); i++) {
        MD * md = m_id2md_map.get(i);
        if (md == NULL) { continue; }
        ASSERT0(MD_id(md) == (UINT)i);
        md->dump(get_type_mgr());
        fflush(g_tfile);
    }
}


//Remove all MDs related to specific variable 'v'.
void MDSystem::removeMDforVAR(VAR const* v, ConstMDIter & iter)
{
    ASSERT0(v);
    MDTab * mdtab = get_md_tab(v);
    if (mdtab != NULL) {
        MD const* x = mdtab->get_effect_md();
        if (x != NULL) {
            MD * freeone = get_md(MD_id(x));
            freeMD(freeone);
        }

        OffsetTab * ofstab = mdtab->get_ofst_tab();
        ASSERT0(ofstab);
        if (ofstab->get_elem_count() > 0) {
            iter.clean();
            for (MD const* md = ofstab->get_first(iter, NULL);
                 md != NULL; md = ofstab->get_next(iter, NULL)) {
                MD * freeone = get_md(MD_id(md));
                freeMD(freeone);
            }
        }
        delete mdtab;
    }
    m_var2mdtab.remove(v);
}
//END MDSystem

} //namespace xoc
