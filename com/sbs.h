/*@
Copyright (c) 2013-2021, Su Zhenyu steven.known@gmail.com

All rights reserved.

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

THIS SOFTWARE IS PROVIDED "AS IS" AND ANY
EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE CONTRIBUTORS BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
@*/
#ifndef __SPARSE_BITSET_H__
#define __SPARSE_BITSET_H__

namespace xcom {

#ifdef _DEBUG_
#define DEBUG_SEG
#endif

#ifdef DEBUG_SEG
#define EXEC_LOG(c) ASSERT0(c)
#else
#define EXEC_LOG(c)
#endif

#define BITS_PER_SEG 512

class BitSet;
class BitSetMgr;
template <UINT BitsPerSeg> class MiscBitSetMgr;

//Templated SEG iter.
#define TSEGIter SC<SEG<BitsPerSeg>*>

//
//Sparse BitSet
//
//Segment of Sparse BitSet.
//Each segment indicates a BitSet.
template <UINT BitsPerSeg = BITS_PER_SEG>
class SEG {
public:
    BSIdx start;
    BitSet bs;
    #ifdef DEBUG_SEG
    UINT id;
    UINT gid;
    #endif

public:
    SEG() { clean(); }
    SEG(SEG const& src) { copy(src); }

    inline void copy(SEG const& src)
    {
        start = src.start;
        bs.copy(src.bs);
    }
    //Count memory usage for current object.
    size_t count_mem() const { return sizeof(start) + bs.count_mem(); }
    void clean() { start = 0; bs.clean(); }

    inline bool is_contain(BSIdx elem)
    {
        if (elem < start) { return false; }
        BSIdx last = start + MAX(bs.get_byte_size(), BYTES_PER_UNIT) *
                     BITS_PER_BYTE - 1;
        if (elem <= last) {
            return true;
        }
        return false;
    }

    //Return the start position of current segment.
    BSIdx get_start() const { return start; }

    //Return the end position of current segment.
    BSIdx get_end() const { return start + BitsPerSeg - 1; }
};


//The class is used to debug allocation and free of SegMgr.
extern UINT g_segmgr_log_count;
template <UINT BitsPerSeg = BITS_PER_SEG>
class SegMgrLog {
public:
    //Record the object that is allocated by current mgr.
    TTab<void*> m_segtab;
    UINT seg_count;
    BSVec<SEG<BitsPerSeg> const*> allocated;
    static UINT g_seg_count;

public:
    bool allocExist(SEG<BitsPerSeg> const* s)
    {
        allocated.set(s->id, s);
        return true;
    }
    bool allocNew(SEG<BitsPerSeg> * s)
    {
        seg_count++;
        g_segmgr_log_count++;
        s->id = seg_count;
        s->gid = g_segmgr_log_count;
        allocated.set(s->id, s);
        recordObj(s);
        return true;
    }

    //Return true if 'obj' is allocated by current mgr.
    //The function always used in Debug Mode.
    bool belongToCurrentMgr(void * obj) const { return m_segtab.find(obj); }

    bool checkLeak(UINT recycled_seg_num)
    {
        ////////////////////////////////////////////////////////////
        //NOTE: SBitSet or SBitSetCore's clean() should be invoked//
        //before destruction, otherwise it will lead SegMgr       //
        //to complain leaking.                                    //
        ////////////////////////////////////////////////////////////
        if (seg_count != recycled_seg_num) {
            //NOTE if leaking happened, usually m_free_list number should less
            //than seg_count, otherwise user intermingled someother unmanaged
            //segment that is not allocated by current segment mgr.
            dump();
            ASSERTN(0, ("MEMLEAK! THERE ARE STILL SEGS UNFREED"));
        }
        m_segtab.clean();
        return true;
    }

    //Dump Segs to help find leaks.
    void dump()
    {
        FileObj f("dump_seg.txt");
        FILE * h = f.getFileHandler();
        fprintf(h, "\n==---- DUMP NOT FREED SEG: ----==");
        fprintf(h, "\nTOTAL SEG COUNT:%u", g_segmgr_log_count);
        fprintf(h, "\nCURRENT SEG COUNT:%u", seg_count);
        for (UINT i = 0; i <= seg_count; i++) {
            if (allocated.is_contain(i)) {
                //Seg i still not be freed.
                SEG<BitsPerSeg> const* s = allocated.get(i);
                ASSERT0(s->id == i);
                fprintf(h, "\n  LEAKED SEG:<id:%d,gid:%d>", s->id, s->gid);
            }
        }
    }

    //Decrease seg_count.
    void dec_seg_count() { seg_count--; }

    bool destroySeg(SEG<BitsPerSeg> const* s)
    {
        dec_seg_count();
        allocated.remove(s->id);
        return true;
    }

    bool freeSeg(SEG<BitsPerSeg> const* s)
    {
        ASSERT0(belongToCurrentMgr(const_cast<void*>((void const*)s)));
        allocated.remove(s->id);
        return true;
    }

    UINT get_seg_count() const { return seg_count; }

    bool init()
    {
        seg_count = 0;
        allocated.clean();
        m_segtab.clean();
        return true;
    }

    //Record 'obj' is allocated by current mgr.
    //The function always used in Debug Mode.
    bool recordObj(void * obj) { m_segtab.append(obj); return true; }
};


//Segment Manager.
//This class is responsible to allocate and destroy SEG object.
//Note this class only handle Default SEG.
template <UINT BitsPerSeg = BITS_PER_SEG>
class SegMgr {
    COPY_CONSTRUCTOR(SegMgr);
    MiscBitSetMgr<BitsPerSeg> * m_sbs_mgr;
    SList<SEG<BitsPerSeg>*> m_free_list;

public:
    #ifdef DEBUG_SEG
    SegMgrLog<BitsPerSeg> logger;
    #endif
public:
    SegMgr(MiscBitSetMgr<BitsPerSeg> * mgr) { m_sbs_mgr = mgr; init(); }
    ~SegMgr() { destroy(); }

    SEG<BitsPerSeg> * allocSEG()
    {
        SEG<BitsPerSeg> * s = m_free_list.remove_head();
        if (s != nullptr) {
            EXEC_LOG(logger.allocExist(s));
            return s;
        }
        s = new SEG<BitsPerSeg>();
        EXEC_LOG(logger.allocNew(s));
        return s;
    }

    void init()
    {
        if (m_free_list.get_pool() != nullptr) { return; }
        EXEC_LOG(logger.init());
        SMemPool * p = smpoolCreate(sizeof(TSEGIter) * 4, MEM_CONST_SIZE);
        m_free_list.set_pool(p);
    }

    void destroy()
    {
        if (m_free_list.get_pool() == nullptr) { return; }

        EXEC_LOG(logger.checkLeak(m_free_list.get_elem_count()));

        for (TSEGIter * sc = m_free_list.get_head();
             sc != m_free_list.end(); sc = m_free_list.get_next(sc)) {
            SEG<BitsPerSeg> * s = sc->val();
            ASSERT0(s);
            delete s;
        }

        //Note member of m_free_list is allocated in pool which is in
        //m_free_list. Thus delete pool at first.
        m_free_list.clean();
        ASSERTN(m_free_list.get_pool(), ("miss pool"));
        smpoolDelete(m_free_list.get_pool());
        m_free_list.set_pool(nullptr);
    }

    void freeSEG(SEG<BitsPerSeg> * s)
    {
        EXEC_LOG(logger.freeSeg(s));
        s->clean();
        m_free_list.append_head(s);
    }

    //Count memory usage for current object.
    size_t count_mem() const
    {
        size_t count = 0;
        for (TSEGIter * sc = m_free_list.get_head();
             sc != m_free_list.end(); sc = m_free_list.get_next(sc)) {
            SEG<BitsPerSeg> * s = sc->val();
            ASSERT0(s);
            count += s->count_mem();
        }

        count += m_free_list.count_mem();
        return count;
    }

    SList<SEG<BitsPerSeg>*> const* get_free_list() const
    { return &m_free_list; }
    MiscBitSetMgr<BitsPerSeg> * getSBSMgr() const { return m_sbs_mgr; }
};


//Sparse BitSet Core
//e.g1:
//    MiscBitSetMgr<33> mbsm;
//    SBitSetCore<33> * x = mbsm.allocSBitSetCore() ;
//    x->bunion(100, mbsm);
//    mbsm.freeSBitSetCore(x); //Very Important!
//e.g2:
//    MiscBitSetMgr<33> mbsm;
//    SBitSetCore<33> x;
//    x.bunion(100, mbsm);
//    x.clean(mbsm);  //Very Important!
template <UINT BitsPerSeg = BITS_PER_SEG>
class SBitSetCore {
    COPY_CONSTRUCTOR(SBitSetCore);
protected:
    SListEx<SEG<BitsPerSeg>*> segs;

    void diff(BSIdx elem, SegMgr<BitsPerSeg> * sm, TSEGIter ** free_list,
              TSEGIter * prev_sct, TSEGIter * sct);

    void * realloc(IN void * src, size_t orgsize, size_t newsize);
public:
    SBitSetCore() {}
    ~SBitSetCore()
    {
        //should call clean() before destruction,
        //otherwise it will incur SegMgr assertion.
    }

    void bunion(SBitSetCore<BitsPerSeg> const& src, SegMgr<BitsPerSeg> * sm,
                TSEGIter ** free_list, SMemPool * pool);
    void bunion(BSIdx elem, SegMgr<BitsPerSeg> * sm, TSEGIter ** free_list,
                SMemPool * pool);
    void bunion(BSIdx elem, MiscBitSetMgr<BitsPerSeg> & m)
    { bunion(elem, &m.sm, &m.scflst, m.ptr_pool); }

    void bunion(SBitSetCore<BitsPerSeg> const& src,
                MiscBitSetMgr<BitsPerSeg> & m)
    { bunion(src, &m.sm, &m.scflst, m.ptr_pool); }

    void clean(MiscBitSetMgr<BitsPerSeg> & m) { clean(&m.sm, &m.scflst); }
    void clean(SegMgr<BitsPerSeg> * sm, TSEGIter ** free_list);
    void copy(SBitSetCore<BitsPerSeg> const& src, SegMgr<BitsPerSeg> * sm,
              TSEGIter ** free_list, SMemPool * pool)
    {
        ASSERTN(this != &src, ("operate on same set"));
        clean(sm, free_list);
        for (TSEGIter * st = src.segs.get_head();
             st != src.segs.end(); st = src.segs.get_next(st)) {
            SEG<BitsPerSeg> * s = st->val();
            ASSERT0(s);

            SEG<BitsPerSeg> * t = sm->allocSEG();
            t->copy(*s);
            segs.append_tail(t, free_list, pool);
        }
    }

    void copy(SBitSetCore<BitsPerSeg> const& src,
              MiscBitSetMgr<BitsPerSeg> & m)
    { copy(src, &m.sm, &m.scflst, m.ptr_pool); }
    //Count memory usage for current object.
    size_t count_mem() const;

    void destroySEGandClean(SegMgr<BitsPerSeg> * sm, TSEGIter ** free_list);

    //Differetiate single 'elem' from current bitset.
    //The simplest method to diff element.
    void diff(BSIdx elem, MiscBitSetMgr<BitsPerSeg> & m)
    { diff(elem, &m.sm, &m.scflst); }

    //Differetiate single 'elem' from current bitset.
    void diff(BSIdx elem, SegMgr<BitsPerSeg> * sm, TSEGIter ** free_list);

    //Differetiate single 'elem' from current bitset.
    //The fastest method to diff element, and you have to provid previous
    //iterator and current iterator.
    //prev_sct: previous iterator
    //sct: current iterator
    void diff(BSIdx elem, TSEGIter * prev_sct, TSEGIter * sct,
              MiscBitSetMgr<BitsPerSeg> & m)
    { diff(elem, &m.sm, &m.scflst, prev_sct, sct); }

    //Differetiate bitset 'src' from current bitset.
    void diff(SBitSetCore<BitsPerSeg> const& src, SegMgr<BitsPerSeg> * sm,
              TSEGIter ** free_list);

    //Differetiate bitset 'src' from current bitset.
    void diff(SBitSetCore<BitsPerSeg> const& src,
              MiscBitSetMgr<BitsPerSeg> & m)
    { diff(src, &m.sm, &m.scflst); }

    void dump(FILE * h) const;
    void dump2(FILE * h) const;

    UINT get_elem_count() const;
    BSIdx get_first(TSEGIter ** cur) const;
    BSIdx get_last(TSEGIter ** cur) const;
    BSIdx get_next(BSIdx elem, TSEGIter ** cur) const;

    void init() { segs.init(); }
    void intersect(SBitSetCore<BitsPerSeg> const& src, SegMgr<BitsPerSeg> * sm,
                   TSEGIter ** free_list);
    void intersect(SBitSetCore<BitsPerSeg> const& src,
                   MiscBitSetMgr<BitsPerSeg> & m)
    { intersect(src, &m.sm, &m.scflst); }

    bool is_equal(SBitSetCore<BitsPerSeg> const& src) const;
    bool is_contain(BSIdx elem) const;
    bool is_contain(SBitSetCore<BitsPerSeg> const& src) const;
    bool is_intersect(SBitSetCore<BitsPerSeg> const& src) const;
    bool is_empty() const;
};


//Sparse BitSet
//This class encapsulates operations of SBitSetCore, and
//simply the usage of them.
//e.g1:
//    MiscBitSetMgr<47> mbsm;
//    SBitSet<47> x(mbsm.getSegMgr());
//    x.bunion(100);
//    //No need to explicit invoke x.clean() to free resource, destructor of x
//    //will do it automatically.
//
//e.g2:
//    MiscBitSetMgr<47> mbsm;
//    SBitSet<47> * x = new SBitSet<47>(mbsm.getSegMgr());
//    x->bunion(100);
//    x->clean(); //Very Important! User should free resource if without
//                //explicit deleting or destroy of x.
template <UINT BitsPerSeg = BITS_PER_SEG>
class SBitSet : public SBitSetCore<BitsPerSeg> {
    COPY_CONSTRUCTOR(SBitSet);
protected:
    SMemPool * m_pool;
    TSEGIter * m_flst; //free list
    SegMgr<BitsPerSeg> * m_sm;
public:
    SBitSet(SegMgr<BitsPerSeg> * sm, UINT sz = sizeof(TSEGIter))
    {
        m_pool = nullptr;
        init(sm, sz);
    }
    ~SBitSet() { destroy(); }

    void init(SegMgr<BitsPerSeg> * sm, UINT sz = sizeof(TSEGIter))
    {
        ASSERTN(sm, ("need SegMgr"));
        ASSERTN(sz % sizeof(TSEGIter) == 0, ("pool size must be mulitple."));
        ASSERTN(m_pool == nullptr, ("already initialized"));
        m_pool = smpoolCreate(sz, MEM_CONST_SIZE);
        m_sm = sm;
        m_flst = nullptr;
    }

    void destroy()
    {
        ASSERTN(m_pool, ("already destroy"));
        for (TSEGIter * st = SBitSetCore<BitsPerSeg>::segs.get_head();
             st != SBitSetCore<BitsPerSeg>::segs.end();
             st = SBitSetCore<BitsPerSeg>::segs.get_next(st)) {
            SEG<BitsPerSeg> * s = st->val();
            ASSERT0(s);

            m_sm->freeSEG(s);
        }

        //Unnecessary call clean(), since free pool will free all
        //SEGIter object.
        //SBitSetCore::clean(m_sm, &m_flst);
        smpoolDelete(m_pool);
        m_pool = nullptr;
    }

    void bunion(BitSet const& src);
    void bunion(SBitSet<BitsPerSeg> const& src)
    { SBitSetCore<BitsPerSeg>::bunion(src, m_sm, &m_flst, m_pool);    }
    void bunion(BSIdx elem)
    { SBitSetCore<BitsPerSeg>::bunion(elem, m_sm, &m_flst, m_pool); }

    void clean() { SBitSetCore<BitsPerSeg>::clean(m_sm, &m_flst); }
    void copy(SBitSet<BitsPerSeg> const& src)
    {
        //Do NOT change current m_sm.
        SBitSetCore<BitsPerSeg>::copy(src, m_sm, &m_flst, m_pool);
    }
    void copy(SBitSetCore<BitsPerSeg> const& src)
    {
        //Do NOT change current m_sm.
        SBitSetCore<BitsPerSeg>::copy(src, m_sm, &m_flst, m_pool);
    }
    //Count memory usage for current object.
    size_t count_mem() const;

    void diff(BSIdx elem)
    { SBitSetCore<BitsPerSeg>::diff(elem, m_sm, &m_flst); }

    //Difference between current bitset and 'src', current bitset
    //will be modified.
    void diff(SBitSet<BitsPerSeg> const& src)
    { SBitSetCore<BitsPerSeg>::diff(src, m_sm, &m_flst); }

    //Do intersection for current bitset and 'src', current bitset
    //will be modified.
    void intersect(SBitSet<BitsPerSeg> const& src)
    { SBitSetCore<BitsPerSeg>::intersect(src, m_sm, &m_flst); }
};


//
//START DBitSetCore, Dual Sparse or Dense BitSetCore
//
//This class represent a BitSet which can be transformed
//in between sparse and dense bitset.
//e.g1:
//    MiscBitSetMgr<47> mbsm;
//    DBitSetCore<47> x;
//    x.set_sparse(True or False);
//    x.bunion(100, mbsm);
//    x.clean(mbsm); //Very Important!
//e.g2:
//    MiscBitSetMgr<47> mbsm;
//    DBitSetCore<47> * x = mbsm.allocDBitSetCore();
//    x->set_sparse(True or False);
//    x->bunion(100, mbsm);
//    mbsm->freeDBitSet(x); //Very Important!
template <UINT BitsPerSeg = BITS_PER_SEG>
class DBitSetCore : public SBitSetCore<BitsPerSeg> {
    COPY_CONSTRUCTOR(DBitSetCore);
protected:
    BYTE m_is_sparse:1; //true if bitset is sparse.
protected:
    //Only read BitSet.
    BitSet const* read_bs() const
    {
        ASSERTN(!m_is_sparse, ("only used by dense bitset"));
        TSEGIter * sc = SBitSetCore<BitsPerSeg>::segs.get_head();
        if (sc == SBitSetCore<BitsPerSeg>::segs.end()) {
            return nullptr;
        }
        ASSERT0(sc->val());
        return &sc->val()->bs;
    }

    //Get BitSet, alloc BitSet if it not exist.
    BitSet * alloc_bs(SegMgr<BitsPerSeg> * sm, TSEGIter ** flst,
                      SMemPool * pool)
    {
        ASSERTN(!m_is_sparse, ("only used by dense bitset"));
        TSEGIter * sc = SBitSetCore<BitsPerSeg>::segs.get_head();
        if (sc == SBitSetCore<BitsPerSeg>::segs.end()) {
            SEG<BitsPerSeg> * t = sm->allocSEG();
            SBitSetCore<BitsPerSeg>::segs.append_head(t, flst, pool);
            return &t->bs;
        }
        ASSERT0(sc->val());
        return &sc->val()->bs;
    }

    //Get BitSet and modify BitSet, do not alloc.
    BitSet * get_bs()
    {
        ASSERTN(!m_is_sparse, ("only used by dense bitset"));
        TSEGIter * sc = SBitSetCore<BitsPerSeg>::segs.get_head();
        if (sc == SBitSetCore<BitsPerSeg>::segs.end()) {
            return nullptr;
        }
        ASSERT0(sc->val());
        return &sc->val()->bs;
    }
public:
    DBitSetCore() { m_is_sparse = true; }
    DBitSetCore(bool is_sparse) { set_sparse(is_sparse); }
    ~DBitSetCore() {}

    void bunion(DBitSetCore<BitsPerSeg> const& src, SegMgr<BitsPerSeg> * sm,
                TSEGIter ** free_list, SMemPool * pool)
    {
        ASSERTN(this != &src, ("operate on same set"));
        ASSERTN(m_is_sparse == src.m_is_sparse, ("diff set type"));
        if (m_is_sparse) {
            SBitSetCore<BitsPerSeg>::bunion(src, sm, free_list, pool);
            return;
        }
        BitSet const* srcbs = src.read_bs();
        if (srcbs == nullptr) { return; }
        BitSet * tgtbs = alloc_bs(sm, free_list, pool);
        tgtbs->bunion(*srcbs);
    }

    void bunion(DBitSetCore<BitsPerSeg> const& src,
                MiscBitSetMgr<BitsPerSeg> & m)
    { bunion(src, &m.sm, &m.scflst, m.ptr_pool); }

    void bunion(BSIdx elem, MiscBitSetMgr<BitsPerSeg> & m)
    { bunion(elem, &m.sm, &m.scflst, m.ptr_pool); }

    void copy(DBitSetCore<BitsPerSeg> const& src,
              MiscBitSetMgr<BitsPerSeg> & m)
    { copy(src, &m.sm, &m.scflst, m.ptr_pool); }

    void diff(BSIdx elem, MiscBitSetMgr<BitsPerSeg> & m)
    { diff(elem, &m.sm, &m.scflst); }

    void diff(DBitSetCore<BitsPerSeg> const& src,
              MiscBitSetMgr<BitsPerSeg> & m)
    { diff(src, &m.sm, &m.scflst); }

    void bunion(BSIdx elem, SegMgr<BitsPerSeg> * sm, TSEGIter ** free_list,
                SMemPool * pool)
    {
        if (m_is_sparse) {
            SBitSetCore<BitsPerSeg>::bunion(elem, sm, free_list, pool);
            return;
        }
        BitSet * tgtbs = alloc_bs(sm, free_list, pool);
        tgtbs->bunion(elem);
    }

    void copy(DBitSetCore<BitsPerSeg> const& src, SegMgr<BitsPerSeg> * sm,
              TSEGIter ** free_list, SMemPool * pool)
    {
        ASSERTN(this != &src, ("operate on same set"));
        ASSERTN(m_is_sparse == src.m_is_sparse, ("diff set type"));
        if (m_is_sparse) {
            SBitSetCore<BitsPerSeg>::copy(src, sm, free_list, pool);
            return;
        }
        BitSet const* srcbs = src.read_bs();
        if (srcbs == nullptr) {
            SBitSetCore<BitsPerSeg>::clean(sm, free_list);
            return;
        }
        BitSet * tgtbs = alloc_bs(sm, free_list, pool);
        tgtbs->copy(*srcbs);
    }
    //Count memory usage for current object.
    size_t count_mem() const
    { return SBitSetCore<BitsPerSeg>::count_mem() + 1; }

    void diff(BSIdx elem, SegMgr<BitsPerSeg> * sm, TSEGIter ** free_list)
    {
        if (m_is_sparse) {
            SBitSetCore<BitsPerSeg>::diff(elem, sm, free_list);
            return;
        }
        BitSet * tgtbs = get_bs();
        if (tgtbs == nullptr) { return; }
        tgtbs->diff(elem);
    }
    void diff(DBitSetCore<BitsPerSeg> const& src, SegMgr<BitsPerSeg> * sm,
              TSEGIter ** free_list)
    {
        ASSERTN(this != &src, ("operate on same set"));
        ASSERTN(m_is_sparse == src.m_is_sparse, ("diff set type"));
        if (m_is_sparse) {
            SBitSetCore<BitsPerSeg>::diff(src, sm, free_list);
            return;
        }
        BitSet const* srcbs = src.read_bs();
        if (srcbs == nullptr) { return; }
        BitSet * tgtbs = get_bs();
        if (tgtbs == nullptr) { return; }
        tgtbs->diff(*srcbs);
    }

    void intersect(DBitSetCore<BitsPerSeg> const& src,
                   MiscBitSetMgr<BitsPerSeg> & m)
    { intersect(src, &m.sm, &m.scflst); }

    void intersect(DBitSetCore<BitsPerSeg> const& src,
                   SegMgr<BitsPerSeg> * sm, TSEGIter ** free_list)
    {
        ASSERTN(this != &src, ("operate on same set"));
        ASSERTN(m_is_sparse == src.m_is_sparse, ("diff set type"));
        if (m_is_sparse) {
            SBitSetCore<BitsPerSeg>::intersect(src, sm, free_list);
            return;
        }
        BitSet const* srcbs = src.read_bs();
        if (srcbs == nullptr) {
            SBitSetCore<BitsPerSeg>::clean(sm, free_list);
            return;
        }
        BitSet * tgtbs = get_bs();
        if (tgtbs == nullptr) { return; }
        tgtbs->intersect(*srcbs);
    }

    bool is_contain(BSIdx elem) const
    {
        if (m_is_sparse) {
            return SBitSetCore<BitsPerSeg>::is_contain(elem);
        }
        BitSet const* tgtbs = read_bs();
        if (tgtbs == nullptr) { return false; }
        return tgtbs->is_contain(elem);
    }

    bool is_equal(DBitSetCore<BitsPerSeg> const& src) const
    {
        ASSERTN(this != &src, ("operate on same set"));
        ASSERTN(m_is_sparse == src.m_is_sparse, ("diff set type"));
        if (m_is_sparse) {
            return SBitSetCore<BitsPerSeg>::is_equal(src);
        }
        BitSet const* srcbs = src.read_bs();
        BitSet const* tgtbs = read_bs();
        if (srcbs == nullptr) {
            if (tgtbs == nullptr) { return true; }
            if (tgtbs->is_empty()) { return true; }
            return false;
        }
        if (tgtbs == nullptr) {
            if (srcbs->is_empty()) { return true; }
            return false;
        }
        return tgtbs->is_equal(*srcbs);
    }

    //*cur will be set to nullptr if set is empty.
    BSIdx get_first(TSEGIter ** cur) const
    {
        ASSERT0(cur);

        TSEGIter * sc = SBitSetCore<BitsPerSeg>::segs.get_head();
        if (sc == SBitSetCore<BitsPerSeg>::segs.end()) {
            ASSERT0(SBitSetCore<BitsPerSeg>::segs.get_elem_count() == 0);
            *cur = nullptr;
            return BS_UNDEF;
        }

        *cur = sc;
        ASSERT0(sc->val());
        SEG<BitsPerSeg> * s = sc->val();

        //DBitSetCore allow bs is empty if it is not sparse.
        //ASSERT0(!s->bs.is_empty());
        return s->get_start() + s->bs.get_first();
    }

    //*cur will be set to nullptr if set is empty.
    BSIdx get_last(TSEGIter ** cur) const
    {
        TSEGIter * sc = SBitSetCore<BitsPerSeg>::segs.get_tail();
        if (sc == SBitSetCore<BitsPerSeg>::segs.end()) {
            ASSERT0(SBitSetCore<BitsPerSeg>::segs.get_elem_count() == 0);
            *cur = nullptr;
            return BS_UNDEF;
        }

        ASSERT0(cur);
        *cur = sc;
        ASSERT0(sc->val());

        SEG<BitsPerSeg> * s = sc->val();

        //DBitSetCore allow bs is empty if it is not sparse.
        //ASSERT0(!s->bs.is_empty());
        return s->get_start() + s->bs.get_last();
    }

    void set_sparse(bool is_sparse) { m_is_sparse = (UINT)is_sparse; }
};


//
//START DBitSet, Dual Sparse or Dense BitSet
//
//This class represent a BitSet which can be transformed in between sparse and
//dense bitset.
//This class encapsulates operations of DBitSetCore, and
//simply the use of them.
//e.g1:
//    MiscBitSetMgr<47> mbsm;
//    DBitSet<47> x(mbsm);
//    x.set_sparse(True or False);
//    x.bunion(100);
//    x.clean(mbsm); //Very Important!
//e.g2:
//    MiscBitSetMgr<47> mbsm;
//    DBitSet<47> * x = mbsm.allocDBitSet();
//    x->set_sparse(True or False);
//    x->bunion(100);
//    mbsm.freeDBitSet(x); //Very Important!
template <UINT BitsPerSeg = BITS_PER_SEG>
class DBitSet : public DBitSetCore<BitsPerSeg> {
    COPY_CONSTRUCTOR(DBitSet);
protected:
    SMemPool * m_pool;
    TSEGIter * m_flst;
    SegMgr<BitsPerSeg> * m_sm;
public:
    DBitSet(SegMgr<BitsPerSeg> * sm, UINT sz = sizeof(TSEGIter))
    {
        ASSERTN(sm, ("need SegMgr"));
        ASSERTN(sz % sizeof(TSEGIter) == 0, ("pool size must be mulitple."));
        DBitSetCore<BitsPerSeg>::m_is_sparse = true;
        m_pool = smpoolCreate(sz, MEM_CONST_SIZE);
        m_sm = sm;
        m_flst = nullptr;
    }
    ~DBitSet()
    {
        for (TSEGIter * st = SBitSetCore<BitsPerSeg>::segs.get_head();
             st != SBitSetCore<BitsPerSeg>::segs.end();
             st = SBitSetCore<BitsPerSeg>::segs.get_next(st)) {
            SEG<BitsPerSeg> * s = st->val();
            ASSERT0(s);

            m_sm->freeSEG(s);
        }

        //Unnecessary call clean(), since free pool will free all
        //SEGIter object.
        //DBitSetCore::clean(m_sm, &m_flst);
        smpoolDelete(m_pool);
    }

    void bunion(DBitSet<BitsPerSeg> const& src)
    { DBitSetCore<BitsPerSeg>::bunion(src, m_sm, &m_flst, m_pool);    }

    void bunion(BSIdx elem)
    { DBitSetCore<BitsPerSeg>::bunion(elem, m_sm, &m_flst, m_pool);    }

    void copy(DBitSet<BitsPerSeg> const& src)
    { DBitSetCore<BitsPerSeg>::copy(src, m_sm, &m_flst, m_pool); }
    //Count memory usage for current object.
    size_t count_mem() const
    {
        size_t count = sizeof(m_pool);
        count += sizeof(m_flst);
        count += sizeof(m_sm);
        count += smpoolGetPoolSize(m_pool);
        count += DBitSetCore<BitsPerSeg>::count_mem();
        return count;
    }

    void clean() { DBitSetCore<BitsPerSeg>::clean(m_sm, &m_flst); }

    void diff(BSIdx elem)
    { DBitSetCore<BitsPerSeg>::diff(elem, m_sm, &m_flst); }
    void diff(DBitSet<BitsPerSeg> const& src)
    { DBitSetCore<BitsPerSeg>::diff(src, m_sm, &m_flst); }

    void intersect(DBitSet<BitsPerSeg> const& src)
    { DBitSetCore<BitsPerSeg>::intersect(src, m_sm, &m_flst); }
};


//This class represent a BitSet Manager that is response for creating
//and destory dense bitset, sparse bitset and dual bitset.
#define MiscBitSetMgr_sc_free_list(sbs) ((sbs)->scflst)

template <UINT BitsPerSeg = BITS_PER_SEG>
class MiscBitSetMgr {
    friend class SegMgr<BitsPerSeg>;
    friend class SBitSetCore<BitsPerSeg>;
    friend class SBitSet<BitsPerSeg>;
    friend class DBitSet<BitsPerSeg>;
    COPY_CONSTRUCTOR(MiscBitSetMgr);
protected:
    SList<SBitSet<BitsPerSeg>*> m_sbitset_list;
    SList<DBitSet<BitsPerSeg>*> m_dbitset_list;
    SList<SBitSetCore<BitsPerSeg>*> m_free_sbitsetcore_list;
    SList<SBitSet<BitsPerSeg>*> m_free_sbitset_list;
    SList<DBitSet<BitsPerSeg>*> m_free_dbitset_list;
    SList<DBitSetCore<BitsPerSeg>*> m_free_dbitsetcore_list;
    SMemPool * m_sbitsetcore_pool;
    SMemPool * m_dbitsetcore_pool;
    TTab<void const*> m_settab; //record the object that is allocated by
                                //current mgr.
protected:
    //Return true if 'obj' is allocated by current mgr.
    //The function always used in Debug Mode.
    bool belongToCurrentMgr(void const* obj) const
    { return m_settab.find(obj); }
    
    //Record 'obj' is allocated by current mgr.
    //The function always used in Debug Mode.
    bool recordObj(void const* obj) { m_settab.append(obj); return true; }

    MiscBitSetMgr * self() { return this; }

    SBitSetCore<BitsPerSeg> * xmalloc_sbitsetc()
    {
        ASSERTN(m_sbitsetcore_pool, ("not yet initialized."));
        SBitSetCore<BitsPerSeg> * p =
            (SBitSetCore<BitsPerSeg>*)smpoolMallocConstSize(
                sizeof(SBitSetCore<BitsPerSeg>), m_sbitsetcore_pool);
        ASSERTN(p, ("malloc failed"));
        ::memset(p, 0, sizeof(SBitSetCore<BitsPerSeg>));
        return p;
    }

    DBitSetCore<BitsPerSeg> * xmalloc_dbitsetc()
    {
        ASSERTN(m_dbitsetcore_pool, ("not yet initialized."));
        DBitSetCore<BitsPerSeg> * p =
            (DBitSetCore<BitsPerSeg>*)smpoolMallocConstSize(
                sizeof(DBitSetCore<BitsPerSeg>), m_dbitsetcore_pool);
        ASSERTN(p, ("malloc failed"));
        ::memset(p, 0, sizeof(DBitSetCore<BitsPerSeg>));
        return p;
    }
public:
    //SEG manager.
    SegMgr<BitsPerSeg> sm;

    //Free list of SEGIter container. It will be allocated in ptr_pool.
    TSEGIter * scflst;

    //Only used to allocate SEGIter.
    SMemPool * ptr_pool;

public:
    MiscBitSetMgr() : sm(self()) { ptr_pool = nullptr; init(); }
    ~MiscBitSetMgr() { destroy(); }

    void init()
    {
        if (ptr_pool != nullptr) { return; }

        ptr_pool = smpoolCreate(sizeof(TSEGIter) * 10, MEM_CONST_SIZE);
        m_sbitsetcore_pool = smpoolCreate(sizeof(SBitSetCore<BitsPerSeg>) * 10,
                                          MEM_CONST_SIZE);
        m_dbitsetcore_pool = smpoolCreate(sizeof(DBitSetCore<BitsPerSeg>) * 10,
                                          MEM_CONST_SIZE);

        m_sbitset_list.set_pool(ptr_pool);
        m_dbitset_list.set_pool(ptr_pool);
        m_free_sbitsetcore_list.set_pool(ptr_pool);
        m_free_sbitset_list.set_pool(ptr_pool);
        m_free_dbitset_list.set_pool(ptr_pool);
        m_free_dbitsetcore_list.set_pool(ptr_pool);

        scflst = nullptr;
        sm.init();
    }

    void destroy()
    {
        if (ptr_pool == nullptr) { return; }

        for (SC<SBitSet<BitsPerSeg>*> * st = m_sbitset_list.get_head();
             st != m_sbitset_list.end();
             st = m_sbitset_list.get_next(st)) {
            SBitSet<BitsPerSeg> * s = st->val();
            ASSERT0(s);
            delete s;
        }

        for (SC<DBitSet<BitsPerSeg>*> * dt = m_dbitset_list.get_head();
             dt != m_dbitset_list.end();
             dt = m_dbitset_list.get_next(dt)) {
            DBitSet<BitsPerSeg> * d = dt->val();
            ASSERT0(d);
            delete d;
        }

        m_sbitset_list.clean();
        m_dbitset_list.clean();
        m_free_sbitset_list.clean();
        m_free_dbitset_list.clean();
        m_free_sbitsetcore_list.clean();
        m_free_dbitsetcore_list.clean();

        smpoolDelete(m_sbitsetcore_pool);
        smpoolDelete(m_dbitsetcore_pool);
        smpoolDelete(ptr_pool);
        sm.destroy();
        ptr_pool = nullptr;
        m_sbitsetcore_pool = nullptr;
        m_dbitsetcore_pool = nullptr;
        scflst = nullptr;
    }

    inline SBitSet<BitsPerSeg> * allocSBitSet()
    {
        SBitSet<BitsPerSeg> * p = m_free_sbitset_list.remove_head();
        if (p == nullptr) {
            p = new SBitSet<BitsPerSeg>(&sm);
            m_sbitset_list.append_head(p);
        }
        ASSERT0(recordObj((void*)p));
        return p;
    }

    inline SBitSetCore<BitsPerSeg> * allocSBitSetCore()
    {
        SBitSetCore<BitsPerSeg> * p = m_free_sbitsetcore_list.remove_head();
        if (p == nullptr) {
            p = xmalloc_sbitsetc();
        }
        ASSERT0(recordObj((void*)p));
        return p;
    }

    inline DBitSet<BitsPerSeg> * allocDBitSet()
    {
        DBitSet<BitsPerSeg> * p = m_free_dbitset_list.remove_head();
        if (p == nullptr) {
            p = new DBitSet<BitsPerSeg>(&sm);
            m_dbitset_list.append_head(p);
        }
        ASSERT0(recordObj((void*)p));
        return p;
    }

    inline DBitSetCore<BitsPerSeg> * allocDBitSetCore()
    {
        DBitSetCore<BitsPerSeg> * p = m_free_dbitsetcore_list.remove_head();
        if (p == nullptr) {
            p = xmalloc_dbitsetc();
            p->set_sparse(true);
        }
        ASSERT0(recordObj((void*)p));
        return p;
    }

    //Note that this function does not add up the memory allocated by
    //allocSBitSetCore() and allocDBitSetCore(). You should count these
    //objects additionally.
    size_t count_mem(FILE * h = nullptr) const;

    //free bs for next use.
    inline void freeSBitSet(SBitSet<BitsPerSeg> * bs)
    {
        if (bs == nullptr) { return; }
        ASSERT0(belongToCurrentMgr((void*)bs));
        bs->clean();
        m_free_sbitset_list.append_head(bs);
    }

    //Free bs for next use.
    inline void freeSBitSetCore(SBitSetCore<BitsPerSeg> * bs)
    {
        if (bs == nullptr) { return; }
        ASSERT0(belongToCurrentMgr((void*)bs));
        bs->clean(*this);
        m_free_sbitsetcore_list.append_head(bs);
    }

    //Free bs for next use.
    inline void freeDBitSet(DBitSet<BitsPerSeg> * bs)
    {
        if (bs == nullptr) { return; }
        ASSERT0(belongToCurrentMgr((void*)bs));
        bs->clean();
        m_free_dbitset_list.append_head(bs);
    }

    //free bs for next use.
    inline void freeDBitSetCore(DBitSetCore<BitsPerSeg> * bs)
    {
        if (bs == nullptr) { return; }
        ASSERT0(belongToCurrentMgr((void*)bs));
        bs->clean(&sm, &scflst);
        m_free_dbitsetcore_list.append_head(bs);
    }

    //This function destroy SEG objects and free containers back to
    //MiscBitSetMgr for next use.
    inline void destroySEGandFreeDBitSetCore(DBitSetCore<BitsPerSeg> * bs)
    {
        if (bs == nullptr) { return; }
        ASSERT0(belongToCurrentMgr((void*)bs));
        bs->destroySEGandClean(&sm, &scflst);

        //Recycle bitset.
        m_free_dbitsetcore_list.append_head(bs);
    }

    SegMgr<BitsPerSeg> * getSegMgr() { return &sm; }
};
//END MiscBitSetMgr


typedef SEG<> DefSEG; //Default Size SEG.
typedef SegMgr<> DefSegMgr;
typedef SBitSetCore<> DefSBitSetCore; //Default Size SBitSetCore.
typedef SBitSet<> DefSBitSet; //Default Size SBitSet.
typedef DBitSetCore<> DefDBitSetCore;
typedef DBitSet<> DefDBitSet;
typedef MiscBitSetMgr<> DefMiscBitSetMgr;

#include "sbs.impl"

//If you want to use different size SEG, then declare the new iter.
typedef SC<DefSEG*> SEGIter; //Default SEG iter.

//Note the iterator of DefSBitSetCore, DBitSet, DBitSetCore are
//same with DefSBitSet.
typedef SEGIter * DefSBitSetIter; //Default size SBitSet iter.
} //namespace xcom

#endif
