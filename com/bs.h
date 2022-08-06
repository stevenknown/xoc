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
#ifndef __BITSET_H__
#define __BITSET_H__

namespace xcom {

#define BS_ZERO 0
#define BYTES_PER_UNIT sizeof(BSUNIT)
#define IS_BSUNDEF(x) ((BSIdx)x == BS_UNDEF)

enum BS_DUMP_FLAG {
    BS_DUMP_BITSET = 0x1,
    BS_DUMP_POS = 0x2,
};

typedef UINT BSUNIT;

class BitSet;
class BitSetMgr;

class BitSet {
    friend BitSet * bs_union(BitSet const& set1, BitSet const& set2,
                             OUT BitSet & res);
    friend BitSet * bs_diff(BitSet const& set1, BitSet const& set2,
                            OUT BitSet & res);
    friend BitSet * bs_intersect(BitSet const& set1, BitSet const& set2,
                                 OUT BitSet & res);
protected:
    UINT m_size;
    BYTE * m_ptr;
protected:
    void * realloc(IN void * src, size_t orgsize, size_t newsize);
public:
    BitSet(UINT init_pool_size = 1)
    {
        m_ptr = 0;
        init(init_pool_size);
    }
    BitSet(BitSet const& bs)
    {
        m_ptr = 0;
        init();
        copy(bs);
    }
    BitSet const& operator = (BitSet const& src);
    ~BitSet() { destroy(); }

    //Initialize bit buffer.
    void init(UINT init_pool_size = 1)
    {
        if (m_ptr != nullptr) { return; }
        m_size = init_pool_size;
        if (init_pool_size == 0) { return; }
        m_ptr = (BYTE*)::malloc(init_pool_size);
        ::memset(m_ptr, 0, m_size);
    }

    //Destroy bit buffer memory.
    void destroy()
    {
        if (m_ptr == nullptr) { return; }
        ASSERTN(m_size > 0, ("bitset is invalid"));
        ::free(m_ptr);
        m_ptr = nullptr;
        m_size = 0;
    }

    //Allocate bytes
    void alloc(UINT size);

    //Returns a new set which is the union of set1 and set2,
    //and modify set1 as result operand.
    void bunion(BitSet const& bs);

    //Add a element which corresponding to 'elem' bit, and set this bit.
    void bunion(BSIdx elem);

    //Do copy from 'src' to 'des'.
    void copy(BitSet const& src);

    //Clean data in buffer with no memory deleted.
    void clean();

    //Count total memory the bitset allocated.
    size_t count_mem() const { return get_byte_size() + sizeof(BitSet); }

    //Complement set of s = univers - s.
    void complement(BitSet const& univers);

    //The difference operation calculates the elements that
    //distinguish one set from another.
    //Remove a element which map with 'elem' bit, and clean this bit.
    void diff(BSIdx elem);

    //The difference operation calculates the elements that
    //distinguish one set from another.
    //Subtracting set2 from set1
    //Returns a new set which is
    //  { x : member( x, 'set1' ) & ~ member( x, 'set2' ) }.
    void diff(BitSet const& bs);

    //Dump bit value and position.
    void dump(CHAR const* name = nullptr, bool is_del = false,
              UFlag f = UFlag(BS_DUMP_BITSET|BS_DUMP_POS),
              BSIdx last_pos = BS_UNDEF) const;

    //Dump bit value and position.
    void dump(FILE * h, UFlag flag, BSIdx last_pos) const;

    //Dump bit value and position.
    void dump(FILE * h) const
    { dump(h, UFlag(BS_DUMP_BITSET|BS_DUMP_POS), BS_UNDEF); }

    //Return the element count in 'set'
    //Add up the population count of each byte in the set.  We get the
    //population counts from the table above.  Great for a machine with
    //effecient loadbyte instructions.
    UINT get_elem_count() const;

    //Return position of first element, start from '0'.
    //Return BS_UNDEF if the bitset is empty.
    BSIdx get_first() const;

    //Get bit postition of the last element ONE.
    //Return BS_UNDEF if bitset is empty.
    BSIdx get_last() const;

    //Extract subset in range between 'low' and 'high'.
    BitSet * get_subset_in_range(BSIdx low, BSIdx high, OUT BitSet & subset);

    //Get bit position of next element ONE to 'elem'.
    //Return BS_UNDEF if it has no other element.
    //'elem': return next one to current element.
    BSIdx get_next(BSIdx elem) const;

    //Get byte size the bitset allocated.
    UINT get_byte_size() const { return m_size; }

    //Get buffer pointer.
    BYTE const* get_byte_vec() const { return m_ptr; }

    //Return true if there is elements in the range between 'low' and 'high'.
    bool has_elem_in_range(BSIdx low, BSIdx high) const;

    //Returns the a new set which is intersection of 'set1' and 'set2'.
    void intersect(BitSet const& bs);

    //Return true if all elements in current bitset is equal to 'bs'.
    bool is_equal(BitSet const& bs) const;

    //Return true if this contain elem.
    bool is_contain(BSIdx elem) const;

    //Return true if 'this' contains 'bs'.
    //'strict': If it is false, we say the bitset contains bs;
    //if it is true, the bitset must have at least one
    //element that does not belong to 'bs'.
    bool is_contain(BitSet const& bs, bool strict = false) const;

    //Return true if 'this' contained in range between 'low' and 'high'.
    //'strict': 'this' strictly contained in range.
    bool is_contained_in_range(BSIdx low, BSIdx high, bool strict) const;

    //Return true if 'this' contained range between 'low' and 'high'.
    bool is_contain_range(BSIdx low, BSIdx high, bool strict) const;

    //Return true if current is intersect with 'bs'.
    bool is_intersect(BitSet const& bs) const;

    //Return true if range between first_bit of 'this' and
    //last_bit of 'this' overlapped with the range between
    //'low' and 'high'.
    bool is_overlapped(BSIdx low, BSIdx high) const;

    //Return true if there is no element ONE in bitset.
    bool is_empty() const;

    //Set each byte in BitSet to 'val'.
    void set(BYTE val) { ::memset(m_ptr, val, m_size); }

    //Reverse each bit.
    //e.g: 1001 to 0110
    //'last_bit_pos': start at 0, e.g:given '101', last bit pos is 2.
    void rev(BSIdx last_bit_pos);
};


//Read Only BitSet.
class ROBitSet : public BitSet {
    COPY_CONSTRUCTOR(ROBitSet);
public:
    ROBitSet(BYTE const* vec, UINT veclen) : BitSet(0) { init(vec, veclen); }
    ~ROBitSet() { m_ptr = nullptr; m_size = 0; }

    //Count memory usage for current object.
    size_t count_mem() const { return get_byte_size(); }

    //Dump bit value and position.
    void dump(CHAR const* name = nullptr, bool is_del = false,
              UFlag f = UFlag(BS_DUMP_BITSET|BS_DUMP_POS),
              BSIdx last_pos = BS_UNDEF) const
    { BitSet::dump(name, is_del, f, last_pos); }
    //Dump bit value and position.
    void dump(FILE * h, UFlag flag, BSIdx last_pos) const
    { BitSet::dump(h, flag, last_pos); }
    //Dump bit value and position.
    void dump(FILE * h) const { BitSet::dump(h); }

    void init(BYTE const* vec, UINT veclen)
    {
        m_size = veclen;
        m_ptr = const_cast<BYTE*>(vec);
    }

    ////////////////////////////////////////////////////////////
    //NOTE: THE FOLLOWING INTERFACES ARE PROHIBITED TO BE USED//
    ////////////////////////////////////////////////////////////
    void rev(BSIdx) { UNREACHABLE(); }
    void intersect(BitSet const&) { UNREACHABLE(); }
    void diff(BSIdx) { UNREACHABLE(); }
    void diff(BitSet const&) { UNREACHABLE(); }
    void copy(BitSet const&) { UNREACHABLE(); }
    void clean() { UNREACHABLE(); }
    void complement(BitSet const&) { UNREACHABLE(); }
    void alloc(UINT) { UNREACHABLE(); }
    void bunion(BitSet const&) { UNREACHABLE(); }
    void bunion(BSIdx) { UNREACHABLE(); }
};


class BitSetMgr {
    COPY_CONSTRUCTOR(BitSetMgr);
protected:
    SMemPool * m_pool;
    List<BitSet*> m_bs_list;
    List<BitSet*> m_free_list;
protected:
    inline void * xmalloc(size_t size)
    {
        ASSERTN(m_pool, ("not yet initialized."));
        void * p = smpoolMallocConstSize(size, m_pool);
        ASSERTN(p, ("malloc failed"));
        ::memset(p, 0, size);
        return p;
    }
public:
    BitSetMgr()
    {
        ASSERT0(sizeof(BSUNIT) >= sizeof(BSIdx));
        m_pool = nullptr;
        init();
    }
    ~BitSetMgr() { destroy(); }

    BitSet * create(UINT init_sz = 0)
    {
        ASSERTN(m_pool, ("not yet init"));
        BitSet * p = m_free_list.remove_head();
        if (p == nullptr) {
            p = (BitSet*)xmalloc(sizeof(BitSet));
            p->init(init_sz);
            m_bs_list.append_head(p);
        }
        return p;
    }

    inline BitSet * copy(BitSet const& bs)
    {
        ASSERTN(m_pool, ("not yet init"));
        BitSet * p = create();
        p->copy(bs);
        return p;
    }

    inline void clean()
    {
        ASSERTN(m_pool, ("not yet init"));
        destroy();
        init();
    }

    //Count memory usage for current object.
    size_t count_mem(FILE * h = nullptr) const;

    void destroy()
    {
        if (m_pool == nullptr) { return; }
        C<BitSet*> * ct;
        for (m_bs_list.get_head(&ct);
             ct != m_bs_list.end(); ct = m_bs_list.get_next(ct)) {
            BitSet * bs = ct->val();
            ASSERT0(bs);
            bs->destroy();
        }
        m_bs_list.destroy();
        m_free_list.destroy();
        smpoolDelete(m_pool);
        m_pool = nullptr;
    }

    inline void free(IN BitSet * bs) //free bs for next use.
    {
        if (bs == nullptr) { return; }

        #ifdef _DEBUG_
        C<BitSet*> * ct;
        for (m_free_list.get_head(&ct);
             ct != m_free_list.end(); ct = m_free_list.get_next(ct)) {
            BitSet * x = ct->val();
            ASSERTN(x && x != bs, ("Already have been freed."));
        }
        #endif
        bs->destroy();
        bs->init();
        m_free_list.append_head(bs);
    }

    inline void init()
    {
        if (m_pool != nullptr) { return; }
        m_pool = smpoolCreate(sizeof(BitSet) * 4, MEM_CONST_SIZE);
        m_bs_list.init();
        m_free_list.init();
    }
};


//
//START BSVec
//
//This class represents a Vector that supply the
//operations like BitSet. You can iterate the element
//in the Vector via get_first and get_next.
template <class T> class BSVec : public Vector<T> {
protected:
    BitSet m_bs; //Record position set by 'set()'
public:
    BSVec() { init(); }
    BSVec(UINT size) { init(); Vector<T>::grow(size); }
    ~BSVec() { destroy(); }

    //Copy element from list.
    inline void copy(List<T> & list)
    {
        ASSERTN(Vector<T>::m_is_init, ("VECTOR not yet initialized."));
        UINT count = 0;

        set(list.get_elem_count() - 1, 0); //Alloc memory right away.

        C<T> * ct;
        for (list.get_head(&ct);
             ct != list.end(); list.get_next(&ct), count++) {
            T elem = ct->val();
            set(count, elem);
        }
    }

    inline void copy(BSVec<T> & vec)
    {
        ASSERTN(Vector<T>::m_is_init, ("VECTOR not yet initialized."));
        Vector<T>::copy(vec);
        m_bs.copy(vec.m_bs);
    }
    //Count memory usage for current object.
    size_t count_mem() const
    { return m_bs.count_mem() + Vector<T>::count_mem(); }
    void clean()
    {
        ASSERTN(Vector<T>::m_is_init, ("VECTOR not yet initialized."));
        Vector<T>::clean();
        m_bs.clean();
    }

    void destroy()
    {
        if (!Vector<T>::m_is_init) { return; }
        m_bs.destroy();
        Vector<T>::destroy();
    }
    void dump(CHAR const* name = nullptr, bool is_del = false) const
    { m_bs.dump(name, is_del); }
    void dump(FILE * h) const { m_bs.dump(h); }

    void init()
    {
        if (Vector<T>::m_is_init) { return; }
        Vector<T>::init();
        m_bs.init();
    }
    bool is_contain(BSIdx i) const { return m_bs.is_contain(i); }

    //Overloaded [] for non-const array reference return.
    //Create an lvalue, equal to 'set()'
    inline T & operator[](INT i)
    {
        ASSERTN(Vector<T>::m_is_init, ("VECTOR not yet initialized."));
        if (i >= Vector<T>::m_size) {
            set(i, (T)0);
        }
        return Vector<T>::m_vec[i];
    }

    //Get the first index number and return the element.
    inline T get_first(OUT BSIdx * idx)
    {
        ASSERTN(Vector<T>::m_is_init, ("VECTOR not yet initialized."));
        BSIdx i = m_bs.get_first();
        if (idx) { *idx = i; }
        return Vector<T>::get(i);
    }

    //Get first number of index of element.
    inline BSIdx get_first() const
    {
        ASSERTN(Vector<T>::m_is_init, ("VECTOR not yet initialized."));
        return m_bs.get_first();
    }

    //Get next index number and return the next element at the same time.
    //Return next element related to current 'idx'.
    inline T get_next(BSIdx * curidx)
    {
        ASSERTN(Vector<T>::m_is_init, ("VECTOR not yet initialized."));
        *curidx = m_bs.get_next(*curidx);
        return Vector<T>::get(*curidx);
    }

    //Get next index number.
    inline BSIdx get_next(BSIdx curidx) const
    {
        ASSERTN(Vector<T>::m_is_init, ("VECTOR not yet initialized."));
        return m_bs.get_next(curidx);
    }

    //Get number of elements in vector.
    UINT get_elem_count() const
    {
        ASSERTN(Vector<T>::m_is_init, ("VECTOR not yet initialized."));
        return m_bs.get_elem_count();
    }

    BitSet * get_bs() { return &m_bs; }

    inline void set(BSIdx i, T elem)
    {
        ASSERTN(Vector<T>::m_is_init, ("VECTOR not yet initialized."));
        Vector<T>::set((VecIdx)i, elem);
        m_bs.bunion(i);
    }

    //Clear bit of position 'i', and set new value 't' for the position.
    //Default placeholder of clear bit is nullptr.
    inline void remove(BSIdx i, T t = (T)0)
    {
        m_bs.diff(i);
        Vector<T>::set((VecIdx)i, t);
    }
};
//END BSVec


//
//START BSVecMgr
//
template <class T> class BSVecMgr {
    COPY_CONSTRUCTOR(BSVecMgr);
protected:
    SList<BSVec<T>*> m_bs_list;
    SList<BSVec<T>*> m_free_list;
    SMemPool * m_pool;
public:
    BSVecMgr() { m_pool = nullptr; init(); }
    ~BSVecMgr(){ destroy(); }

    BSVec<T> * create()
    {
        ASSERTN(m_pool, ("not yet init"));

        BSVec<T> * p = m_free_list.remove_head();
        if (p == nullptr) {
            p = (BSVec<T>*)::malloc(sizeof(BSVec<T>));
            ::memset(p, 0, sizeof(BSVec<T>));
            p->init();
            m_bs_list.append_head(p);
        }
        return p;
    }

    inline void clean()
    {
        ASSERTN(m_pool, ("not yet init"));
        destroy();
        init();
    }
    //Count memory usage for current object.
    size_t count_mem()
    {
        size_t count = smpoolGetPoolSize(m_pool);
        for (SC<BSVec<T>*> * ct = m_bs_list.get_head();
             ct != m_bs_list.end(); ct = m_bs_list.get_next(ct)) {
            BSVec<T> * bs = ct->val();
            ASSERT0(bs);

            count += bs->count_mem();
        }
        return count;
    }

    inline void init()
    {
        if (m_pool != nullptr) { return; }
        m_pool = smpoolCreate(sizeof(SC<BSVec<T>*>) * 2, MEM_CONST_SIZE);
        m_bs_list.set_pool(m_pool);
        m_free_list.set_pool(m_pool);
    }

    void destroy()
    {
        if (m_pool == nullptr) { return; }
        for (SC<BSVec<T>*> * ct = m_bs_list.get_head();
             ct != m_bs_list.end(); ct = m_bs_list.get_next(ct)) {
            BSVec<T> * bs = ct->val();
            ASSERT0(bs);

            bs->destroy();
        }

        smpoolDelete(m_pool);
        m_pool = nullptr;
    }
    void dump(FILE * h);

    inline void free(IN BSVec<T> * bs) //free bs for next use.
    {
        if (bs == nullptr) { return; }
        bs->clean();
        m_free_list.append_head(bs);
    }
};

template <class T> void BSVecMgr<T>::dump(FILE * h)
{
    if (h == nullptr) { return; }

    //Dump mem usage into file.
    List<UINT> lst;
    for (BSVec<T> const* bs = m_bs_list.get_head();
         bs != m_bs_list.end(); bs = m_bs_list.get_next()) {
        size_t c = bs->count_mem();
        C<UINT> * ct;
        UINT n = lst.get_elem_count();
        lst.get_head(&ct);
        UINT i;
        for (i = 0; i < n; i++, lst.get_next(&ct)) {
            if (c >= ct->val()) {
                lst.insert_before(c, ct);
                break;
            }
        }
        if (i == n) {
            lst.append_head(c);
        }
    }
    UINT v = lst.get_head();
    fprintf(h, "\n== DUMP BitSetMgr: total %d "
               "bitsets, mem usage are:\n",
               m_bs_list.get_elem_count());
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
//END BSVecMgr


extern BYTE const g_bit_count[];
extern inline BitSet * bs_create(BitSetMgr & bs_mgr)
{
    return bs_mgr.create();
}
extern BitSet * bs_union(BitSet const& set1, BitSet const& set2,
                         OUT BitSet & res);
extern BitSet * bs_diff(BitSet const& set1, BitSet const& set2,
                        OUT BitSet & res);
extern BitSet * bs_intersect(BitSet const& set1, BitSet const& set2,
                             OUT BitSet & res);

} //namespace xcom
#endif
