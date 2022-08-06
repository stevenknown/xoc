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

class Var;
class MDSystem;
class Region;

typedef enum _MD_TYPE {
    MD_UNBOUND = 0,
    MD_EXACT = 1,
    MD_RANGE = 2,
} MD_TYPE;
typedef UINT MDIdx;

#define MD_UNDEF 0 //Undefined.
#define MD_FULL_MEM 1 //Represent all program memory.
#define MD_FIRST MD_FULL_MEM
#define MD_GLOBAL_VAR 2 //Represent variables that allocated at program region
                        //by explicit definition.
#define MD_IMPORT_VAR 3 //Represent variables allocated at outer region
                        //which nor program region.
#define MD_HEAP_MEM 4 //Represent variables allocated in heap.
#define MD_LOCAL_VAR 5 //Represent variables allocated in current region.
//The first id that is allocable.
#define MD_FIRST_ALLOCABLE (MD_IMPORT_VAR + 1)

//Memory Descriptor.
//MD is an appealing property to represent exact or inexact memory object.
//By using MD, we can model miscellaneous memory accessing, and perform
//memory analysis and optimizations.
//
//MD is used to represent different memory object with
//same base. Attributes of MD may be Id, Base Variable, Size, Offset,
//Effect, Exact, Range, Unbound.
//¡ð Id
//    Unique id of abstract memory object.
//
//¡ð Base Variable
//    Since MD is the abstract version of Var, it is closely related to
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
//¡ð Exact
//    This attribute represent abstract memory object with type is
//    MD_EXACT. An exact MD is also effect.
//
//¡ð Range
//    This attribute represent abstract memory object with type is
//    MD_RANGE. An range MD is also effect, but is not exact.
//
//¡ð Unbound
//    This attribute represent abstract memory object with type is
//    MD_UNBOUND. An unbound MD may be effect, but is definitly inexact.

//Unique id of memory object.
#define MD_id(md) ((md)->uid)

//Each MD has a base, it is corresponding to an unique variable.
#define MD_base(md) ((md)->base)

//Record the offset from base if MD is exact or range, or it is not available.
#define MD_ofst(md) ((md)->ofst)

//Record the byte size of memory object if MD is exact or range,
//or it is not available.
#define MD_size(md) ((md)->size)

//Memory object type. If it is MD_EXACT, there will be exact use/def.
//If it is MD_RANGE, there will be inexact use/def, but the accessing restricted
//into a computable range. If it is MD_UNBOUND, there will be inexact use/def,
//and we do not know where to be access.
#define MD_ty(md) ((md)->u2.s1.type)

//The memory object is a PR.
#define MD_is_pr(md) (MD_base(md)->is_pr())

//True indicates MD will not be effect MD, namely,
//the MD only could be put in MayDef or MayUse md set.
#define MD_is_may(md) ((md)->u2.s1.is_may_reference)

class MD {
public:
    MDIdx uid; //unique id.
    TMWORD ofst; //byte offsets relative to 'base'
    TMWORD size; //byte size of the memory block
    Var * base;
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
    MD const& operator = (MD const&);

    inline void copy(MD const* md)
    {
        ASSERT0(md && this != md);
        MD_base(this) = MD_base(md);
        MD_ofst(this) = MD_ofst(md);
        MD_size(this) = MD_size(md);
        u2.s1v = md->u2.s1v;
    }

    Var * get_base() const { return MD_base(this); }
    TMWORD getBitOfst() const { return MD_ofst(this); }
    TMWORD getByteOfst() const { return MD_ofst(this); }
    TMWORD getByteSize() const { return MD_size(this); }
    MD_TYPE getType() const { return (MD_TYPE)MD_ty(this); }

    MDIdx id() const { return MD_id(this); }
    //Return true if current md may cover 'm', such as:
    //current md: |-...-----...---|
    //m:            |---...-|
    bool is_may_cover(MD const* m) const;

    //Return true if current md exactly cover 'm', such as:
    //CASE1:
    //  current md: |-------|
    //  m:            |----|
    //CASE2:
    //  current md: |----------|
    //  m:            |--...--|
    bool is_exact_cover(MD const* m) const;

    //Return true if current md intersect but may be not cover 'm', such as:
    //current md: |-------|
    //m:            |-------|
    bool is_overlap(MD const* m) const;

    //Return true if md can ONLY be put in MayDef or MayUse.
    //And the md should NOT be effect MD.
    bool is_may() const { return MD_is_may(this); }

    //Return true if md represent real object that would be emitted to
    //target machine. Fake object is not effect object.
    //NOTE: Effect inexact MD represents the memory object which may or may
    //not exist. If some stmt modified effect but inexact MD, it will be
    //non-killing definition.
    bool is_effect() const { return !MD_base(this)->is_fake(); }

    //Return true if md is exact object.
    //Exact MD represent must and killing-DEF or USE.
    bool is_exact() const { return MD_ty(this) == MD_EXACT; }

    //Return true if md is unbound.
    bool is_unbound() const { return MD_ty(this) == MD_UNBOUND; }

    //Return true if md is global variable.
    bool is_global() const { return MD_base(this)->is_global(); }

    //Return true if md is volatile memory.
    bool is_volatile() const { return MD_base(this)->is_volatile(); }

    //Return true if user hint guarranteed that the MD does not overlap
    //with other MDs.
    bool is_restrict() const { return MD_base(this)->is_restrict(); }

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
        MD_base(this) = nullptr;
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
        return (((ULONGLONG)t1->is_range()) |
                (((ULONGLONG)MD_ofst(t1)) << 1) |
                (((ULONGLONG)MD_size(t1)) << 32)) <
               (((ULONGLONG)t2->is_range()) |
                (((ULONGLONG)MD_ofst(t2)) << 1) |
                (((ULONGLONG)MD_size(t2)) << 32));
    }

    bool is_equ(MD const* t1, MD const* t2) const
    {
        ASSERT0(MD_base(t1) == MD_base(t2));
        return (((ULONGLONG)t1->is_range()) |
                (((ULONGLONG)MD_ofst(t1)) << 1) |
                (((ULONGLONG)MD_size(t1)) << 32)) ==
               (((ULONGLONG)t2->is_range()) |
                (((ULONGLONG)MD_ofst(t2)) << 1) |
                (((ULONGLONG)MD_size(t2)) << 32));
    }

    MD const* createKey(MD const* t) { return t; }
};


//MD hashed by MD_ofst.
class OffsetTab : public TMap<MD const*, MD const*, CompareOffset> {
public:
    //Return the entry.
    MD const* find(MD const* md)
    { return TMap<MD const*, MD const*, CompareOffset>::get(md, nullptr); }

    void append(MD const* md)
    { TMap<MD const*, MD const*, CompareOffset>::set(md, md); }
};


//Each Var corresponds to an unqiue MDTab.
class MDTab {
    COPY_CONSTRUCTOR(MDTab);
protected:
    OffsetTab m_ofst_tab;
    MD const* m_invalid_ofst_md; //record MD with invalid ofst

public:
    MDTab() { m_invalid_ofst_md = nullptr; }

    void init(UINT hash_bucket_size);
    void clean()
    {
        m_invalid_ofst_md = nullptr;
        m_ofst_tab.clean();
    }
    //Count memory usage for current object.
    size_t count_mem() const
    { return m_ofst_tab.count_mem() + (size_t)sizeof(m_invalid_ofst_md); }

    MD const* find(MD const* md)
    {
        if (md->is_exact() || md->is_range()) {
            return m_ofst_tab.find(md);
        }
        return m_invalid_ofst_md;
    }

    void append(MD const* md)
    {
        if (md->is_exact() || md->is_range()) {
            m_ofst_tab.append(md);
            return;
        }
        ASSERT0(m_invalid_ofst_md == nullptr);
        m_invalid_ofst_md = md;
    }

    UINT get_elem_count()
    {
        UINT elems = 0;
        if (m_invalid_ofst_md != nullptr) {
            elems++;
        }
        elems += m_ofst_tab.get_elem_count();
        return elems;
    }

    OffsetTab * get_ofst_tab() {  return &m_ofst_tab; }
    MD const* get_effect_md() { return m_invalid_ofst_md; }
    void get_elems(OUT Vector<MD const*> & mdv, ConstMDIter & iter)
    {
        MDIdx idx = MD_UNDEF;
        if (m_invalid_ofst_md != nullptr) {
            mdv.set(idx++, m_invalid_ofst_md);
        }
        for (MD const* md = m_ofst_tab.get_first(iter, nullptr);
             md != nullptr; md = m_ofst_tab.get_next(iter, nullptr)) {
            mdv.set(idx++, md);
        }
    }
};


typedef xcom::SEGIter * MDSetIter;

//Memory Descriptor Set.
//Note: one must call clean() to reclamition before deletion or destruction.
class MDSet : public DefSBitSetCore {
    COPY_CONSTRUCTOR(MDSet);
public:
    MDSet() {}
    ~MDSet() {} //should call clean() before destruction.

    void bunion(MDSet const& pt, DefMiscBitSetMgr & mbsmgr);
    void bunion(MD const* md, DefMiscBitSetMgr & mbsmgr)
    { bunion(MD_id(md), mbsmgr); }
    void bunion(MDIdx mdid, DefMiscBitSetMgr & mbsmgr);
    void bunion_pure(MDIdx mdid, DefMiscBitSetMgr & m)
    { DefSBitSetCore::bunion(mdid, m); }
    void bunion_pure(MDSet const& mds, DefMiscBitSetMgr & m)
    { DefSBitSetCore::bunion(mds, m); }

    bool is_contain_pure(MDIdx mdid) const
    { return DefSBitSetCore::is_contain(mdid); }
    bool is_contain_pure(MDSet const& mds) const
    { return DefSBitSetCore::is_contain(mds); }

    //Return true if set contained global variable.
    bool is_contain_global() const
    {
        return DefSBitSetCore::is_contain(MD_GLOBAL_VAR) ||
               DefSBitSetCore::is_contain(MD_IMPORT_VAR);
               //|| DefSBitSetCore::is_contain(MD_FULL_MEM);
    }

    //Return true if set contained full memory variable.
    bool is_contain_fullmem() const
    {
        //return DefSBitSetCore::is_contain(MD_FULL_MEM);
        return false;
    }

    //Return true if set contained md.
    bool is_contain(MD const* md) const;

    //Return true if set only contained the md that has been taken address.
    bool is_contain_only_taken_addr(MD const* md) const;

    //Return true if md is overlap with the elements in set.
    bool is_overlap(MD const* md, Region const* current_ru) const;

    //Return true if md is overlap with the elements.
    //Note this function only consider the MD that have been taken address.
    bool is_overlap_only_taken_addr(MD const* md,
                                    Region const* current_ru) const;

    //Return true if md overlaps with element in current MDSet.
    //Note this function will iterate all elements which is costly.
    //Use it carefully.
    bool is_overlap_ex(MD const* md, Region const* current_ru,
                       MDSystem const* mdsys) const;
    bool is_contain_inexact(MDSystem const* ms) const;
    bool is_contain_only_exact_and_str(MDSystem const* ms) const;
    //Return true current set is equivalent to mds, whereas every element
    //in set is exact.
    bool is_exact_equal(MDSet const& mds, MDSystem const* ms) const;

    //Return true if set intersect with 'mds'.
    bool is_intersect(MDSet const& mds) const
    {
        ASSERT0(this != &mds);
        //Use function of base class (DefSBitSetCore) instead of.
        //if (DefSBitSetCore::is_contain(MD_GLOBAL_VAR) &&
        //    ((DefSBitSetCore&)mds).is_contain(MD_GLOBAL_VAR)) {
        //    return true;
        //}

        //TBD: Does it necessary to judge if either current
        //MD or input MD is FULL_MEM?
        //As we observed, passes that utilize MD relationship add
        //MD2 to accroding IR's MDSet, which can keep global variables
        //and MD2 dependence.
        //e.g: g=10, #mustdef=MD10, maydef={MD2, MD10}, g is global variable that
        //           #represented in Program Region.
        //     foo(); #maydef={MD2, MD10}
        //if ((DefSBitSetCore::is_contain(MD_FULL_MEM) && !mds.is_empty()) ||
        //    (((DefSBitSetCore&)mds).is_contain(MD_FULL_MEM) &&
        //     !DefSBitSetCore::is_empty())) {
        //    return true;
        //}

        return DefSBitSetCore::is_intersect(mds);
    }

    void diff(MD const* md, DefMiscBitSetMgr & m)
    {
        ASSERT0(md);
        DefSBitSetCore::diff(MD_id(md), m);
    }
    void diff(MDIdx id, DefMiscBitSetMgr & m) { DefSBitSetCore::diff(id, m); }
    void diff(MDSet const& mds, DefMiscBitSetMgr & m)
    {
        ASSERT0(this != &mds);
        ASSERTN(!DefSBitSetCore::is_contain(MD_FULL_MEM), ("low performance"));

        //TBD: Does it necessary to judge if either current
        //MD or input MD is FULL_MEM?
        //As we observed, passes that utilize MD relationship add
        //MD2 to accroding IR's MDSet, which can keep global variables
        //and MD2 dependence.
        //e.g: g=10, #mustdef=MD10, maydef={MD2, MD10}, g is global variable that
        //           #represented in Program Region.
        //     foo(); #maydef={MD2, MD10}
        //if (((DefSBitSetCore const&)mds).is_contain(MD_FULL_MEM)) {
        //    clean(m);
        //    return;
        //}
        DefSBitSetCore::diff(mds, m);
    }
    //This function will walk through whole current MDSet and differenciate
    //overlapped elements.
    //Note this function is very costly.
    void diffAllOverlapped(MDIdx id, DefMiscBitSetMgr & m, MDSystem const* sys);
    void dump(MDSystem * ms, bool detail = false) const;

    //Get unique MD that is effective, and offset must be valid.
    //Note the MDSet can only contain one element.
    //Return the effect MD if found, otherwise return nullptr.
    inline MD * get_exact_md(MDSystem * ms) const
    {
        MD * md = get_effect_md(ms);
        if (md != nullptr && md->is_exact()) {
            return md;
        }
        return nullptr;
    }

    //Get unique MD that is not fake memory object,
    //but its offset might be invalid.
    //Note the MDSet can only contain one element.
    //Return the effect MD if found, otherwise return nullptr.
    MD * get_effect_md(MDSystem * ms) const;

    //Return the unique MD if current set has only one element.
    MD * get_unique_md(MDSystem const* ms) const;
};


//MDSetMgr
class MDSetMgr {
    COPY_CONSTRUCTOR(MDSetMgr);
protected:
    SMemPool * m_mds_pool;
    SMemPool * m_sc_mds_pool;
    SList<MDSet*> m_free_md_set;
    SList<MDSet*> m_md_set_list;
    Region * m_rg;
    DefMiscBitSetMgr * m_misc_bs_mgr;

public:
    MDSetMgr(Region * rg, DefMiscBitSetMgr * mbsm);
    ~MDSetMgr() { destroy(); }

    //Allocate a new MDSet.
    MDSet * alloc()
    {
        MDSet * mds = get_free();
        if (mds == nullptr) {
            mds = (MDSet*)smpoolMallocConstSize(sizeof(MDSet), m_mds_pool);
            ASSERT0(mds);
            ::memset(mds, 0, sizeof(MDSet));
            m_md_set_list.append_head(mds);
        }
        return mds;
    }

    //Count memory usage for current object.
    size_t count_mem() const;

    //Destroy MDSet manager.
    void destroy();
    void dump();

    //Clean and give it back to md set manager.
    //Do not destroy mds.
    void free(MDSet * mds);

    Region * getRegion() const { return m_rg; }
    inline MDSet * get_free() { return m_free_md_set.remove_head(); }
    UINT get_mdset_count() const { return m_md_set_list.get_elem_count(); }
    UINT get_free_mdset_count() const { return m_free_md_set.get_elem_count(); }
};


class MDId2MD : public Vector<MD*> {
    COPY_CONSTRUCTOR(MDId2MD);
    UINT m_count;

public:
    MDId2MD() { m_count = 0; }

    void remove(MDIdx mdid)
    {
        ASSERT0(mdid != 0); //0 is illegal mdid.
        ASSERT0(get(mdid) != nullptr);
        Vector<MD*>::set(mdid, nullptr);
        m_count--;
    }

    void set(MDIdx mdid, MD * md)
    {
        ASSERTN(Vector<MD*>::get(mdid) == nullptr, ("already mapped"));
        Vector<MD*>::set(mdid, md);
        m_count++;
    }

    UINT get_elem_count() const { return m_count; }
    void dump(Region const* rg) const;
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

    void dump(Region * rg);
};


//MD System.
//Manage the memory allocation and free of MD, and
//the mapping between MD_id and MD.
//Manage the memory allocation and free of MDTab, and
//the mapping between Var and MDTab.
//NOTE: each region manager has a single MDSystem.
class MDSystem {
    COPY_CONSTRUCTOR(MDSystem);
    SMemPool * m_pool;
    SMemPool * m_sc_mdptr_pool;
    TypeMgr * m_tm;
    Var * m_all_mem;
    Var * m_global_mem;
    Var * m_import_var;
    MDId2MD m_id2md_map; //Map MD id to MD.
    SList<MD*> m_free_md_list; //MD allocated in pool.
    UINT m_md_count; //generate MD index, used by registerMD().
    typedef TMap<Var const*, MDTab*, CompareConstVar> Var2MDTab;
    typedef TMapIter<Var const*, MDTab*> Var2MDTabIter;
    Var2MDTab m_var2mdtab; //map Var to MDTab.

    inline MD * allocMD()
    {
        MD * md = m_free_md_list.remove_head();
        if (md == nullptr) {
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
    ~MDSystem() { destroy(); }

    void init(VarMgr * vm);
    void clean();
    void computeOverlapExactMD(MD const* md, OUT MDSet * output,
                               ConstMDIter & mditer,
                               DefMiscBitSetMgr & mbsmgr);

    //Compute all other MD which are overlapped with 'md', the output
    //will include 'md' itself if there are overlapped MDs.
    //e.g: given md1, and md1 overlapped with md2, md3,
    //then output set is {md1, md2, md3}.
    //md: input to compute the overlapped md-set.
    //tabiter: for local use.
    //strictly: set to true to compute if md may be overlapped
    //            with global variables or import variables.
    //Note this function does NOT clean output, and will append result to
    //output.
    void computeOverlap(Region * current_ru, MD const* md,
                        OUT MDSet & output, ConstMDIter & mditer,
                        DefMiscBitSetMgr & mbsmgr, bool strictly);

    //Compute all other MD which are overlapped with MD in set 'mds'.
    //e.g: mds contains {md1}, and md1 overlapped with md2, md3,
    //then output set 'mds' is {md1, md2, md3}.
    //mds: it is not only input but also output buffer.
    //added: records the new MD that added into 'mds'.
    //mditer: for local use.
    //strictly: set to true to compute if md may be overlapped with global
    //memory.
    void computeOverlap(Region * current_ru, MOD MDSet & mds,
                        OUT Vector<MD const*> & added, ConstMDIter & mditer,
                        DefMiscBitSetMgr & mbsmgr, bool strictly);

    //Compute all other MD which are overlapped with MD in set 'mds'.
    //e.g: mds contains {md1}, and md1 overlapped with md2, md3,
    //then output is {md2, md3}.
    //mds: it is readonly input.
    //output: output MD set.
    //mditer: for local use.
    //strictly: set to true to compute if MD may be overlapped with global
    //memory.
    //Note output do not need to clean before invoke this function.
    void computeOverlap(Region * current_ru, MDSet const& mds,
                        OUT MDSet & output, ConstMDIter & mditer,
                        DefMiscBitSetMgr & mbsmgr, bool strictly);

    //Dump all registered MDs.
    void dump(bool only_dump_nonpr_md);
    void destroy();

    TypeMgr * getTypeMgr() const { return m_tm; }

    //Get registered MD.
    //NOTICE: DO NOT free the return value, because it is the registered one.
    MD * getMD(MDIdx id) const
    {
        ASSERT0(id != MD_UNDEF);
        MD * md = m_id2md_map.get(id);
        ASSERT0(md == nullptr || MD_id(md) == id);
        return md;
    }

    MD const* readMD(MDIdx id) const
    {
        ASSERT0(id != MD_UNDEF);
        MD * md = m_id2md_map.get(id);
        ASSERT0(md == nullptr || MD_id(md) == id);
        return md;
    }

    //Get MD TAB that described mds which under same base Var.
    MDTab * getMDTab(Var const* v)
    {
        ASSERT0(v);
        return m_var2mdtab.get(v);
    }
    RegionMgr * getRegionMgr() const { return m_tm->getRegionMgr(); }

    UINT getNumOfMD() const { return m_id2md_map.get_elem_count(); }
    MDId2MD const* getID2MDMap() const { return &m_id2md_map; }

    inline void freeMD(MD * md)
    {
        if (md == nullptr) { return; }
        m_id2md_map.remove(MD_id(md));
        MDIdx mdid = MD_id(md);
        ::memset(md, 0, sizeof(MD));
        MD_id(md) = mdid;
        m_free_md_list.append_head(md);
    }

    //Register MD accroding to specific m. And return the generated MD.
    MD const* registerMD(MD const& m);

    //Register an effectively unbound MD that base is 'var'.
    MD const* registerUnboundMD(Var * var, TMWORD size);

    //Remove all MDs related to specific variable 'v'.
    void removeMDforVAR(Var const* v, IN ConstMDIter & iter);
};

typedef TMapIter<MDIdx, MDSet const*> MD2MDSetIter;

//MD2MD_SET_MAP
//Record MD->MDS relations.
//Note MD may mapped to nullptr, means the MD does not point to anything.
class MD2MDSet : public TMap<MDIdx, MDSet const*> {
    COPY_CONSTRUCTOR(MD2MDSet);
public:
    MD2MDSet() {}
    ~MD2MDSet()
    {
        //Note all elements should be in MD_HASH.
        //ASSERT0(get_elem_count() == 0); //should call free first.
    }

    //Clean each MD->MDSet, but do not free MDSet.
    void clean() { TMap<MDIdx, MDSet const*>::clean(); }
    //Compute the number of PtPair that recorded in current MD2MDSet.
    UINT computePtPairNum(MDSystem const& mdsys) const
    {
        UINT num_of_tgt_md = 0;
        MD2MDSetIter mxiter;
        MDSet const* from_md_pts = nullptr;
        for (MDIdx fromid = get_first(mxiter, &from_md_pts);
            fromid > 0; fromid = get_next(mxiter, &from_md_pts)) {
            ASSERT0(const_cast<MDSystem&>(mdsys).getMD(fromid));
            if (from_md_pts == nullptr || from_md_pts->is_contain_fullmem()) {
                continue;
            }
            num_of_tgt_md += from_md_pts->get_elem_count();
        }
        return num_of_tgt_md;
    }

    void dump(Region * rg);
};

inline bool isGlobalSideEffectMD(MDIdx id)
{
    return id == MD_GLOBAL_VAR || id == MD_IMPORT_VAR || id == MD_FULL_MEM;
}

inline bool isContainGlobalSideEffectMD(MDSet const& set)
{
    DefSBitSetCore const& lset = (DefSBitSetCore const&)set;
    return lset.is_contain(MD_GLOBAL_VAR) || lset.is_contain(MD_IMPORT_VAR) ||
           lset.is_contain(MD_FULL_MEM);
}

} //namespace xoc
#endif
