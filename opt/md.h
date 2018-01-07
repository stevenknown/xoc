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
#ifndef _MEM_DESC_H_
#define _MEM_DESC_H_

namespace xoc {

class MDSystem;
class Region;

typedef enum _MD_TYPE {
    MD_UNBOUND = 0,
    MD_EXACT,
    MD_RANGE,
} MD_TYPE;

#define MD_UNDEF              0 //Undefined.
#define MD_FULL_MEM           1 //All program memory.
#define MD_FIRST              MD_FULL_MEM
#define MD_GLOBAL_MEM         2 //Allocate at static data section
                                //BYTE explicit definition.
#define MD_IMPORT_VAR         3 //Not allocated in current region.
#define MD_HEAP_MEM           4 //Allocate in heap.
#define MD_STACK_MEM          5 //Allocate at stack.
//The first id which is allocable.
#define MD_FIRST_ALLOCABLE (MD_IMPORT_VAR + 1)

//Memory Descriptor.
//MD is an appealing property to represent exact or inexact memory object.
//By using MD, we can model miscellaneous memory accessing, and perform
//memory analysis and optimizations.
//
//MD is used to represent different memory object with
//same base. Attributes of MD may be Id, Base Variable, Size, Offset,
//Effect, Exact, Range, Unbound.
//¡ð    Id
//    Unique id of abstract memory object.
//
//¡ð    Base Variable
//    Since MD is the abstract version of VAR, it is closely related to
//    individual variable. This variable may be the base of several MD.
//
//¡ð Type
//    This attribute represent abstract memory object type.
//    * MD_UNBOUND
//        The object is unbound if we have no knowledge about MD size or
//        MD offset. The Def-Use relation to the object is inexact. The
//        store to object is nonkilling definition.
//
//    * MD_EXACT
//        The object is exact indicate the memory address and size is
//        determinate when load or store to the object. The Def-Use
//        relation to the object is exact. In general, the memory load
//        or store will be exact if its data type is primitive.
//        The store to object is killing definition.
//
//    * MD_RANGE
//        The object is range if we both know the MD offset of base
//        variable and MD size, but the precise address and byte size
//        may be uncertain when load or store to the object. The Def-Use
//        relation to the object is inexact. The store to object is
//        nonkilling definition.
//
//¡ð Size
//    This attribute represents byte size of the abstract memory object.
//
//¡ð Offset
//    This attribute represents byte size offset to the base variable.
//
//¡ð Effect
//    This attribute refers to variables which are definitely declared
//    by user or compiler and existed in the concrete. In contrast to
//    effect MD, ALL_MEM memory object is ineffect.
//
//¡ð    Exact
//    This attribute represent abstract memory object with type is
//    MD_EXACT. An exact MD is also effect.
//
//¡ð Range
//    This attribute represent abstract memory object with type is
//    MD_RANGE. An range MD is also effect, but is not exact.
//
//¡ð Unbound
//        This attribute represent abstract memory object with type is
//        MD_UNBOUND. An unbound MD may be effect, but is definitly inexact.

//Unique id of memory object.
#define MD_id(md)                ((md)->uid)

//Each MD has a base, it is corresponding to an unique variable.
#define MD_base(md)              ((md)->base)

//Record the offset from base if MD is exact or range, or it is not available.
#define MD_ofst(md)              ((md)->ofst)

//Record the byte size of memory object if MD is exact or range,
//or it is not available.
#define MD_size(md)              ((md)->size)

//Memory object type. If it is MD_EXACT, there will be exact use/def.
//If it is MD_RANGE, there will be inexact use/def, but the accessing restricted
//into a computable range. If it is MD_UNBOUND, there will be inexact use/def,
//and we do not know where to be access.
#define MD_ty(md)                ((md)->u2.s1.type)

//The memory object is a PR.
#define MD_is_pr(md)             (VAR_is_pr(MD_base(md)))

//True indicates MD will not be effect MD, namely,
//the MD only could be put in MayDef or MayUse md set.
#define MD_is_may(md)            ((md)->u2.s1.is_may_reference)

class MD {
public:
    UINT uid; //unique id.
    UINT ofst; //byte offsets relative to 'base'
    UINT size; //byte size of the memory block
    VAR * base;
    union {
        struct {
            BYTE type:2;
            BYTE is_addr_taken:1;
            BYTE is_may_reference:1;
        } s1;
        BYTE s1v;
    } u2;

    MD() { clean(); }
    MD(MD const& md)
    {
        //Do not copy id.
        MD_id(this) = 0;
        copy(&md);
    }

    inline void copy(MD const* md)
    {
        ASSERT0(md && this != md);
        MD_base(this) = MD_base(md);
        MD_ofst(this) = MD_ofst(md);
        MD_size(this) = MD_size(md);
        u2.s1v = md->u2.s1v;
    }

    VAR * get_base() const { return MD_base(this); }
    UINT get_ofst() const { return MD_ofst(this); }
    UINT get_size() const { return MD_size(this); }
    MD_TYPE get_type() const { return (MD_TYPE)MD_ty(this); }

    UINT id() const { return MD_id(this); }
    //Return true if current md exactly cover 'm', such as:
    //current md: |-------|
    //m:            |----|
    bool is_cover(MD const* m) const;

    //Return true if current md intersect but may be not cover 'm', such as:
    //current md: |-------|
    //m:            |-------|
    bool is_overlap(MD const* m) const;

    //Return true if md represent real object that would be emitted to
    //target machine. Fake object is not effect object.
    //NOTE: Effect MD inexact represent the memory object which may or may
    //not occur at run time. If stmt modified effect but inexact MD,
    //it is non-killing definition.
    bool is_effect() const { return !VAR_is_fake(MD_base(this)); }

    //Return true if md is exact object.
    //Exact MD represent must and killing-DEF or USE.
    bool is_exact() const { return MD_ty(this) == MD_EXACT; }

    //Return true if md is unbound.
    bool is_unbound() const { return MD_ty(this) == MD_UNBOUND; }

    //Return true if md is global variable.
    bool is_global() const { return VAR_is_global(MD_base(this)); }

    //Return true if md is volatile memory.
    bool is_volatile() const { return VAR_is_volatile(MD_base(this)); }

    //If MD is range, MD_base + MD_ofst indicate the start address,
    //MD_size indicate the range.
    bool is_range() const { return MD_ty(this) == MD_RANGE; }

    //Return true if md indicate PR.
    bool is_pr() const { return MD_is_pr(this); }

    //Return true if src is definitly equal to current md.
    bool is_equ(MD const& src) const
    {
        ASSERT0(this != &src);
        return *this == src;
    }

    inline bool operator == (MD const& src) const
    {
        ASSERT0(this != &src);
        if (MD_base(this) != MD_base(&src)) { return false; }
        if (is_unbound() && src.is_unbound()) { return true; }
        return ofst == src.ofst && size == src.size && u2.s1v == src.u2.s1v;
    }

    //Dump md into 'buf', 'bufl' indicate the byte length of the buffer.
    CHAR * dump(StrBuf & buf,  TypeMgr * dm) const;

    //Dump md to file.
    void dump(TypeMgr * dm) const;

    inline void clean()
    {
        MD_id(this) = 0;
        MD_ofst(this) = 0;
        MD_size(this) = 0;
        MD_base(this) = NULL;
        u2.s1v = 0;
    }
};


typedef TMapIter<MD*, MD*> MDIter;
typedef TMapIter<MD const*, MD const*> ConstMDIter;

class CompareOffset {
public:
    bool is_less(MD const* t1, MD const*  t2) const
    {
        ASSERT0(MD_base(t1) == MD_base(t2));
        return (((ULONGLONG)MD_ofst(t1)) << 32 | ((ULONGLONG)MD_size(t1))) <
               (((ULONGLONG)MD_ofst(t2)) << 32 | ((ULONGLONG)MD_size(t2)));
    }

    bool is_equ(MD const* t1, MD const* t2) const
    {
        ASSERT0(MD_base(t1) == MD_base(t2));
        return (((ULONGLONG)MD_ofst(t1)) << 32 | ((ULONGLONG)MD_size(t1))) ==
               (((ULONGLONG)MD_ofst(t2)) << 32 | ((ULONGLONG)MD_size(t2)));
    }

    MD const* createKey(MD const* t) { return t; }
};


//MD hashed by MD_ofst.
class OffsetTab : public TMap<MD const*, MD const*, CompareOffset> {
public:
    //Return the entry.
    MD const* find(MD const* md)
    { return TMap<MD const*, MD const*, CompareOffset>::get(md, NULL); }

    void append(MD const* md)
    { TMap<MD const*, MD const*, CompareOffset>::set(md, md); }
};


//Each VAR corresponds to an unqiue MDTab.
class MDTab {
protected:
    OffsetTab m_ofst_tab;
    MD const* m_invalid_ofst_md; //record MD with invalid ofst
public:
    MDTab() { m_invalid_ofst_md = NULL; }

    void init(UINT hash_bucket_size);
    void clean()
    {
        m_invalid_ofst_md = NULL;
        m_ofst_tab.clean();
    }

    size_t count_mem() const
    { return m_ofst_tab.count_mem() + (size_t)sizeof(m_invalid_ofst_md); }

    MD const* find(MD const* md)
    {
        if (md->is_exact()) {
            return m_ofst_tab.find(md);
        }
        return m_invalid_ofst_md;
    }

    void append(MD const* md)
    {
        if (md->is_exact()) {
            m_ofst_tab.append(md);
            return;
        }
        ASSERT0(m_invalid_ofst_md == NULL);
        m_invalid_ofst_md = md;
    }

    UINT get_elem_count()
    {
        UINT elems = 0;
        if (m_invalid_ofst_md != NULL) {
            elems++;
        }
        elems += m_ofst_tab.get_elem_count();
        return elems;
    }

    OffsetTab * get_ofst_tab() {  return &m_ofst_tab; }
    MD const* get_effect_md() { return m_invalid_ofst_md; }
    void get_elems(OUT Vector<MD const*> & mdv, ConstMDIter & iter)
    {
        UINT idx = 0;
        if (m_invalid_ofst_md != NULL) {
            mdv.set(idx++, m_invalid_ofst_md);
        }
        for (MD const* md = m_ofst_tab.get_first(iter, NULL);
             md != NULL; md = m_ofst_tab.get_next(iter, NULL)) {
            mdv.set(idx++, md);
        }
    }
};


//Memory Descriptor Set.
//Note: one must call clean() to reclamition before deletion or destruction.
class MDSet : public DefSBitSetCore {
public:
    MDSet() {}
    COPY_CONSTRUCTOR(MDSet);
    ~MDSet() {} //should call clean() before destruction.

    void bunion(MDSet const& pt, DefMiscBitSetMgr & mbsmgr);
    void bunion(MD const* md, DefMiscBitSetMgr & mbsmgr);
    void bunion(UINT mdid, DefMiscBitSetMgr & mbsmgr);
    void bunion_pure(UINT mdid, DefMiscBitSetMgr & m)
    { DefSBitSetCore::bunion(mdid, m); }
    void bunion_pure(MDSet const& mds, DefMiscBitSetMgr & m)
    { DefSBitSetCore::bunion(mds, m); }

    bool is_contain_pure(UINT mdid) const
    { return DefSBitSetCore::is_contain(mdid); }

    //Return true if set contain global variable.
    bool is_contain_global() const
    {
        return DefSBitSetCore::is_contain(MD_GLOBAL_MEM) ||
               DefSBitSetCore::is_contain(MD_IMPORT_VAR) ||
               DefSBitSetCore::is_contain(MD_FULL_MEM);
    }

    //Return true if set contain all memory variable.
    bool is_contain_all() const
    { return DefSBitSetCore::is_contain(MD_FULL_MEM); }

    //Return true if set contain md.
    inline bool is_contain(MD const* md) const
    {
        if (DefSBitSetCore::is_contain(MD_GLOBAL_MEM) && md->is_global()) {
            return true;
        }
        if (DefSBitSetCore::is_contain(MD_FULL_MEM)) {
            return true;
        }
        return DefSBitSetCore::is_contain(MD_id(md));
    }

    //Return true if md is overlap with the elements in set.
    bool is_overlap(MD const* md, Region * current_ru) const;

    //Return true if md is overlapped with element in current MDSet.
    //Note this function will iterate elements in set which is costly.
    //Use it carefully.
    bool is_overlap_ex(
            MD const* md,
            Region * current_ru,
            MDSystem const* mdsys) const;

    bool is_contain_inexact(MDSystem * ms) const;
    bool is_contain_only_exact_and_str(MDSystem * ms) const;
    bool is_exact_equal(MDSet const& mds, MDSystem * ms) const;

    //Return true if set intersect with 'mds'.
    inline bool is_intersect(MDSet const& mds) const
    {
        if (this == &mds) { return true; }
        if (DefSBitSetCore::is_contain(MD_GLOBAL_MEM) &&
            ((DefSBitSetCore&)mds).is_contain(MD_GLOBAL_MEM)) {
            return true;
        }

        if ((DefSBitSetCore::is_contain(MD_FULL_MEM) && !mds.is_empty()) ||
            (((DefSBitSetCore&)mds).is_contain(MD_FULL_MEM) &&
             !DefSBitSetCore::is_empty())) {
            return true;
        }
        return DefSBitSetCore::is_intersect(mds);
    }

    //Return true if all MD in set are PR.
    inline bool is_pr_set(MDSystem const* mdsys) const;

    void diff(MD const* md, DefMiscBitSetMgr & m)
    {
        ASSERT0(md);
        DefSBitSetCore::diff(MD_id(md), m);
    }

    void diff(UINT id, DefMiscBitSetMgr & m) { DefSBitSetCore::diff(id, m); }

    inline void diff(MDSet const& mds, DefMiscBitSetMgr & m)
    {
        ASSERT0(this != &mds);
        ASSERT(!DefSBitSetCore::is_contain(MD_FULL_MEM), ("low performance"));
        if (((DefSBitSetCore const&)mds).is_contain(MD_FULL_MEM)) {
            clean(m);
            return;
        }
        DefSBitSetCore::diff(mds, m);
    }
    void dump(MDSystem * ms, bool detail = false) const;

    //Get unique MD that is effective, and offset must be valid.
    inline MD * get_exact_md(MDSystem * ms) const
    {
        MD * md = get_effect_md(ms);
        if (md != NULL && md->is_exact()) {
            return md;
        }
        return NULL;
    }
    MD * get_effect_md(MDSystem * ms) const;
};


//MDSetMgr
class MDSetMgr {
protected:
    SMemPool * m_mds_pool;
    SMemPool * m_sc_mds_pool;
    SList<MDSet*> m_free_md_set;
    SList<MDSet*> m_md_set_list;
    Region * m_ru;
    DefMiscBitSetMgr * m_misc_bs_mgr;
public:
    MDSetMgr(Region * rg, DefMiscBitSetMgr * mbsm);
    COPY_CONSTRUCTOR(MDSetMgr);
    ~MDSetMgr() { destroy(); }

    //Clean and give it back to md set manager.
    //Do not destroy mds.
    void free(MDSet * mds);

    inline MDSet * get_free() { return m_free_md_set.remove_head(); }
    UINT get_mdset_count() const { return m_md_set_list.get_elem_count(); }
    UINT get_free_mdset_count() const { return m_free_md_set.get_elem_count(); }

    //Create new MDSet.
    MDSet * create()
    {
        MDSet * mds = get_free();
        if (mds == NULL) {
            mds = (MDSet*)smpoolMallocConstSize(sizeof(MDSet), m_mds_pool);
            ASSERT0(mds);
            ::memset(mds, 0, sizeof(MDSet));
            m_md_set_list.append_head(mds);
        }
        return mds;
    }

    //Destroy MDSet manager.
    void destroy();
    void dump();

    UINT count_mem();
};


class MDId2MD : public Vector<MD*> {
    UINT m_count;
public:
    MDId2MD() { m_count = 0; }

    void remove(UINT mdid)
    {
        ASSERT0(mdid != 0); //0 is illegal mdid.
        ASSERT0(get(mdid) != NULL);
        Vector<MD*>::set(mdid, NULL);
        m_count--;
    }

    void set(UINT mdid, MD * md)
    {
        ASSERT(Vector<MD*>::get(mdid) == NULL, ("already mapped"));
        Vector<MD*>::set(mdid, md);
        m_count++;
    }

    UINT get_elem_count() const { return m_count; }
    void dump() const;
};


class MDSetHashAllocator {
    MiscBitSetMgr<> * m_sbs_mgr;
public:
    MDSetHashAllocator(MiscBitSetMgr<> * sbsmgr)
    { ASSERT0(sbsmgr); m_sbs_mgr = sbsmgr; }

    SBitSetCore<> * alloc() { return m_sbs_mgr->allocSBitSetCore(); }
    void free(SBitSetCore<> * set) { m_sbs_mgr->freeSBitSetCore(set); }
    MiscBitSetMgr<> * getBsMgr() const { return m_sbs_mgr; }
};


class MDSetHash : public SBitSetCoreHash<MDSetHashAllocator> {
public:
    MDSetHash(MDSetHashAllocator * allocator) :
        SBitSetCoreHash<MDSetHashAllocator>(allocator) {}
    virtual ~MDSetHash() {}

    MDSet const* append(SBitSetCore<> const& set)
    { return (MDSet const*)SBitSetCoreHash<MDSetHashAllocator>::append(set); }

    void dump();
};


//MD System.
//Manage the memory allocation and free of MD, and
//the mapping between MD_id and MD.
//Manage the memory allocation and free of MDTab, and
//the mapping between VAR and MDTab.
//NOTE: each region manager has a single MDSystem.
class MDSystem {
    SMemPool * m_pool;
    SMemPool * m_sc_mdptr_pool;
    TypeMgr * m_tm;
    VAR * m_all_mem;
    VAR * m_global_mem;
    VAR * m_import_var;
    MDId2MD m_id2md_map; //Map MD id to MD.
    SList<MD*> m_free_md_list; //MD allocated in pool.
    UINT m_md_count; //generate MD index, used by registerMD().
    TMap<VAR const*, MDTab*, CompareConstVar> m_var2mdtab; //map VAR to MDTab.

    inline MD * allocMD()
    {
        MD * md = m_free_md_list.remove_head();
        if (md == NULL) {
            md = (MD*)smpoolMallocConstSize(sizeof(MD), m_pool);
            md->clean();
        }
        return md;
    }

    //Allocated object should be recorded in list.
    MDTab * allocMDTab() { return new MDTab(); }
    void initGlobalMemMD(VarMgr * vm);
    void initImportVar(VarMgr * vm);
    void initAllMemMD(VarMgr * vm);
public:
    MDSystem(VarMgr * vm) { init(vm); }
    COPY_CONSTRUCTOR(MDSystem);
    ~MDSystem() { destroy(); }

    void init(VarMgr * vm);
    void clean();
    void computeOverlapExactMD(
            MD const* md,
            OUT MDSet * output,
            ConstMDIter & tabiter,
            DefMiscBitSetMgr & mbsmgr);
    void computeOverlap(
            Region * current_ru,
            MD const* md,
            MDSet & output,
            ConstMDIter & tabiter,
            DefMiscBitSetMgr & mbsmgr,
            bool strictly);
    void computeOverlap(
            Region * current_ru,
            IN OUT MDSet & mds,
            Vector<MD const*> & tmpvec,
            ConstMDIter & tabiter,
            DefMiscBitSetMgr & mbsmgr,
            bool strictly);
    void computeOverlap(
            Region * current_ru,
            MDSet const& mds,
            OUT MDSet & output,
            ConstMDIter & tabiter,
            DefMiscBitSetMgr & mbsmgr,
            bool strictly);

    //Dump all registered MDs.
    void dumpAllMD();
    void destroy();

    TypeMgr * getTypeMgr() const { return m_tm; }

    //Get registered MD.
    //NOTICE: DO NOT free the return value, because it is the registered one.
    MD * getMD(UINT id)
    {
        ASSERT0(id != 0);
        MD * md = m_id2md_map.get(id);
        ASSERT0(md == NULL || MD_id(md) == id);
        return md;
    }

    MD const* readMD(UINT id) const
    {
        ASSERT0(id != 0);
        MD * md = m_id2md_map.get(id);
        ASSERT0(md == NULL || MD_id(md) == id);
        return md;
    }

    //Get MD TAB that described mds which under same base VAR.
    MDTab * getMDTab(VAR const* v)
    {
        ASSERT0(v);
        return m_var2mdtab.get(v);
    }

    UINT getNumOfMD() const { return m_id2md_map.get_elem_count(); }
    MDId2MD const* getID2MDMap() const { return &m_id2md_map; }

    inline void freeMD(MD * md)
    {
        if (md == NULL) { return; }
        m_id2md_map.remove(MD_id(md));
        UINT mdid = MD_id(md);
        ::memset(md, 0, sizeof(MD));
        MD_id(md) = mdid;
        m_free_md_list.append_head(md);
    }

    //Register MD accroding to specific m. And return the generated MD.
    MD const* registerMD(MD const& m);

    //Register an effectively unbound MD that base is 'var'.
    MD const* registerUnboundMD(VAR * var, UINT size);

    //Remove all MDs related to specific variable 'v'.
    void removeMDforVAR(VAR const* v, IN ConstMDIter & iter);
};


//Return true if all MD in set are PR.
bool MDSet::is_pr_set(MDSystem const* mdsys) const
{
    SEGIter * iter;
    for (INT i = get_first(&iter);
         i >= 0; i = get_next((UINT)i, &iter)) {
        MD const* md = mdsys->readMD((UINT)i);
        ASSERT0(md);
        if (!md->is_pr()) { return false; }
    }
    return true;
}


typedef TMapIter<UINT, MDSet const*> MD2MDSetIter;

//MD2MD_SET_MAP
//Record MD->MDS relations.
//Note MD may mapped to NULL, means the MD does not point to anything.
class MD2MDSet : public TMap<UINT, MDSet const*> {
public:
    ~MD2MDSet()
    {
        //Note all elements should be in MD_HASH.
        //ASSERT0(get_elem_count() == 0); //should call free first.
    }

    //Clean each MD->MDSet, but do not free MDSet.
    void clean() { TMap<UINT, MDSet const*>::clean(); }

    void dump(Region * rg);
};
} //namespace xoc
#endif
