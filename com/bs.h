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

#define BS_ZERO           0
#define BS_DUMP_BITSET    1
#define BS_DUMP_POS       2
#define BITS_PER_BYTE     8
#define BYTES_PER_UINT    4

class BitSet;
class BitSetMgr;

class BitSet
{
    friend BitSet * bs_union(BitSet const& set1,
                             BitSet const& set2,
                             OUT BitSet & res);
    friend BitSet * bs_diff(BitSet const& set1,
                            BitSet const& set2,
                            OUT BitSet & res);
    friend BitSet * bs_intersect(BitSet const& set1,
                                 BitSet const& set2,
                                 OUT BitSet & res);
protected:
    UINT m_size;
    BYTE * m_ptr;

    void * realloc(IN void * src, size_t orgsize, size_t newsize);
public:
    BitSet(UINT init_pool_size = 1)
    {
        m_ptr = 0;
        init(init_pool_size);
    }

    //Copy constructor
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
        if (m_ptr != NULL) { return; }
        m_size = init_pool_size;
        if (init_pool_size == 0) { return; }
        m_ptr = (BYTE*)::malloc(init_pool_size);
        ::memset(m_ptr, 0, m_size);
    }

    //Destroy bit buffer memory.
    void destroy()
    {
        if (m_ptr == NULL) return;
        ASSERTN(m_size > 0, ("bitset is invalid"));
        ::free(m_ptr);
        m_ptr = NULL;
        m_size = 0;
    }

    //Allocate bytes
    void alloc(UINT size);

    //Returns a new set which is the union of set1 and set2,
    //and modify set1 as result operand.
    void bunion(BitSet const& bs);

    //Add a element which corresponding to 'elem' bit, and set this bit.
    void bunion(UINT elem);

    //Do copy from 'src' to 'des'.
    void copy(BitSet const& src);

    //Clean data in buffer with no memory deleted.
    void clean();

    //Count total memory the bitset allocated.
    UINT count_mem() const { return get_byte_size() + sizeof(BitSet); }

    //Complement set of s = univers - s.
    void complement(BitSet const& univers);

    //The difference operation calculates the elements that
    //distinguish one set from another.
    //Remove a element which map with 'elem' bit, and clean this bit.
    void diff(UINT elem);

    //The difference operation calculates the elements that
    //distinguish one set from another.
    //Subtracting set2 from set1
    //Returns a new set which is
    //  { x : member( x, 'set1' ) & ~ member( x, 'set2' ) }.
    void diff(BitSet const& bs);
    void dump(CHAR const* name = NULL, bool is_del = false,
              UINT flag = BS_DUMP_BITSET | BS_DUMP_POS,
              INT last_pos = -1) const;
    void dump(FILE * h, UINT flag, INT last_pos) const;
    void dump(FILE * h) const { dump(h, BS_DUMP_BITSET|BS_DUMP_POS, -1); }

    //Return the element count in 'set'
    //Add up the population count of each byte in the set.  We get the
    //population counts from the table above.  Great for a machine with
    //effecient loadbyte instructions.
    UINT get_elem_count() const;

    //Return position of first element, start from '0'.
    //Return -1 if the bitset is empty.
    INT get_first() const;

    //Get bit postition of the last element ONE.
    //Return -1 if bitset is empty.
    INT get_last() const;

    //Extract subset in range between 'low' and 'high'.
    BitSet * get_subset_in_range(UINT low, UINT high, OUT BitSet & subset);

    //Get bit position of next element ONE to 'elem'.
    //Return -1 if it has no other element.
    //'elem': return next one to current element.
    INT get_next(UINT elem) const;

    //Get byte size the bitset allocated.
    UINT get_byte_size() const { return m_size; }

    //Get buffer pointer.
    BYTE const* get_byte_vec() const { return m_ptr; }

    //Return true if there is elements in the range between 'low' and 'high'.
    bool has_elem_in_range(UINT low, UINT high) const;

    //Returns the a new set which is intersection of 'set1' and 'set2'.
    void intersect(BitSet const& bs);

    //Return true if all elements in current bitset is equal to 'bs'.
    bool is_equal(BitSet const& bs) const;

    //Return true if this contain elem.
    bool is_contain(UINT elem) const;

    //Return true if 'this' contains 'bs'.
    //'strict': If it is false, we say the bitset contains bs;
    //if it is true, the bitset must have at least one
    //element that does not belong to 'bs'.
    bool is_contain(BitSet const& bs, bool strict = false) const;

    //Return true if 'this' contained in range between 'low' and 'high'.
    //'strict': 'this' strictly contained in range.
    bool is_contained_in_range(UINT low, UINT high, bool strict) const;

    //Return true if 'this' contained range between 'low' and 'high'.
    bool is_contain_range(UINT low, UINT high, bool strict) const;

    //Return true if current is intersect with 'bs'.
    bool is_intersect(BitSet const& bs) const;

    //Return true if range between first_bit of 'this' and
    //last_bit of 'this' overlapped with the range between
    //'low' and 'high'.
    bool is_overlapped(UINT low, UINT high) const;

    //Return true if there is no element ONE in bitset.
    bool is_empty() const;

    //Reverse each bit.
    //e.g: 1001 to 0110
    //'last_bit_pos': start at 0, e.g:given '101', last bit pos is 2.
    void rev(UINT last_bit_pos);
};


//Read Only BitSet.
class ROBitSet : public BitSet {
public:
    ROBitSet(BYTE const* vec, UINT veclen) : BitSet(0) { init(vec, veclen); }
    COPY_CONSTRUCTOR(ROBitSet);
    ~ROBitSet() { m_ptr = NULL; m_size = 0; }

    void init(BYTE const* vec, UINT veclen)
    {
        m_size = veclen;
        m_ptr = const_cast<BYTE*>(vec);
    }

    UINT count_mem() const { return get_byte_size(); }

    void dump(CHAR const* name = NULL, bool is_del = false,
              UINT flag = BS_DUMP_BITSET | BS_DUMP_POS,
              INT last_pos = -1) const
    { BitSet::dump(name, is_del, flag, last_pos); }

    void dump(FILE * h, UINT flag, INT last_pos) const
    { BitSet::dump(h, flag, last_pos); }

    void dump(FILE * h) const { BitSet::dump(h); }

    //The followed interfaces are prohibited to use.
    void rev(UINT last_bit_pos);
    void intersect(BitSet const& bs);
    void diff(UINT elem);
    void diff(BitSet const& bs);
    void copy(BitSet const& src);
    void clean();
    void complement(BitSet const& univers);
    void alloc(UINT size);
    void bunion(BitSet const& bs);
    void bunion(UINT elem);
};


class BitSetMgr
{
protected:
    SMemPool * m_pool;
    List<BitSet*> m_bs_list;
    List<BitSet*> m_free_list;

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
        m_pool = NULL;
        init();
    }
    COPY_CONSTRUCTOR(BitSetMgr);
    ~BitSetMgr() { destroy(); }


    inline void init()
    {
        if (m_pool != NULL) { return; }
        m_pool = smpoolCreate(sizeof(BitSet) * 4, MEM_CONST_SIZE);
        m_bs_list.init();
        m_free_list.init();
    }

    void destroy()
    {
        if (m_pool == NULL) { return; }

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
        m_pool = NULL;
    }

    BitSet * create(UINT init_sz = 0)
    {
        ASSERTN(m_pool, ("not yet init"));
        BitSet * p = m_free_list.remove_head();
        if (p == NULL) {
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
    UINT count_mem(FILE * h = NULL);

    inline void free(IN BitSet * bs) //free bs for next use.
    {
        if (bs == NULL) { return; }

        #ifdef _DEBUG_
        C<BitSet*> * ct;
        for (m_free_list.get_head(&ct);
             ct != m_free_list.end(); ct = m_free_list.get_next(ct)) {
            BitSet * x = ct->val();
            ASSERTN(x && x != bs, ("Already have been freed."));
        }
        #endif
        bs->clean();
        m_free_list.append_head(bs);
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
    BSVec(INT size)
    {
        init();
        Vector<T>::grow(size);
    }
    ~BSVec() { destroy(); }

    void init()
    {
        if (Vector<T>::m_is_init) return;
        Vector<T>::init();
        m_bs.init();
    }

    void destroy()
    {
        if (!Vector<T>::m_is_init) return;
        m_bs.destroy();
        Vector<T>::destroy();
    }

    //Copy element from list.
    inline void copy(List<T> & list)
    {
        ASSERTN(Vector<T>::m_is_init, ("VECTOR not yet initialized."));
        INT count = 0;

        set(list.get_elem_count()-1, 0); //Alloc memory right away.

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

    UINT count_mem() const { return m_bs.count_mem() + Vector<T>::count_mem(); }
    inline void clean()
    {
        ASSERTN(Vector<T>::m_is_init, ("VECTOR not yet initialized."));
        Vector<T>::clean();
        m_bs.clean();
    }

    bool is_contain(UINT i) const { return m_bs.is_contain(i); }

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
    inline T get_first(OUT INT * idx)
    {
        ASSERTN(Vector<T>::m_is_init, ("VECTOR not yet initialized."));
        INT i = m_bs.get_first();
        if (idx) { *idx = i; }
        return Vector<T>::get(i);
    }

    //Get first number of index of element.
    inline INT get_first() const
    {
        ASSERTN(Vector<T>::m_is_init, ("VECTOR not yet initialized."));
        return m_bs.get_first();
    }

    //Get next index number and return the next element at the same time.
    //Return next element related to current 'idx'.
    inline T get_next(INT * curidx)
    {
        ASSERTN(Vector<T>::m_is_init, ("VECTOR not yet initialized."));
        *curidx = m_bs.get_next(*curidx);
        return Vector<T>::get(*curidx);
    }

    //Get next index number.
    inline INT get_next(UINT curidx) const
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

    inline void set(UINT i, T elem)
    {
        ASSERTN(Vector<T>::m_is_init, ("VECTOR not yet initialized."));
        Vector<T>::set(i, elem);
        m_bs.bunion(i);
    }

    //Clear bit of position 'i', and set new value 't' for the position.
    //Default placeholder of clear bit is NULL.
    inline void remove(UINT i, T t = (T)0)
    {
        m_bs.diff(i);
        Vector<T>::set(i, t);
    }

    void dump(CHAR const* name = NULL, bool is_del = false) const
    { m_bs.dump(name, is_del); }

    void dump(FILE * h) const
    { m_bs.dump(h); }
};
//END BSVec


//
//START BSVecMgr
//
template <class T> class BSVecMgr {
protected:
    SList<BSVec<T>*> m_bs_list;
    SList<BSVec<T>*> m_free_list;
    SMemPool * m_pool;

public:
    BSVecMgr()
    {
        m_pool = NULL;
        init();
    }
    COPY_CONSTRUCTOR(BSVecMgr);
    ~BSVecMgr(){ destroy(); }

    inline void init()
    {
        if (m_pool != NULL) { return; }
        m_pool = smpoolCreate(sizeof(SC<BSVec<T>*>) * 2, MEM_CONST_SIZE);
        m_bs_list.set_pool(m_pool);
        m_free_list.set_pool(m_pool);
    }

    void destroy()
    {
        if (m_pool == NULL) { return; }

        for (SC<BSVec<T>*> * ct = m_bs_list.get_head();
             ct != m_bs_list.end(); ct = m_bs_list.get_next(ct)) {
            BSVec<T> * bs = ct->val();
            ASSERT0(bs);

            bs->destroy();
        }

        smpoolDelete(m_pool);
        m_pool = NULL;
    }

    void dump(FILE * h)
    {
        if (h == NULL) { return; }

        //Dump mem usage into file.
        List<UINT> lst;
        for (BSVec<T> const* bs = m_bs_list.get_head();
             bs != m_bs_list.end(); bs = m_bs_list.get_next()) {
            UINT c = bs->count_mem();
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

    BSVec<T> * create()
    {
        ASSERTN(m_pool, ("not yet init"));

        BSVec<T> * p = m_free_list.remove_head();
        if (p == NULL) {
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

    inline void free(IN BSVec<T> * bs) //free bs for next use.
    {
        if (bs == NULL) return;
        bs->clean();
        m_free_list.append_head(bs);
    }
};


extern BYTE const g_bit_count[];
extern inline BitSet * bs_create(BitSetMgr & bs_mgr)
{
    return bs_mgr.create();
}
extern BitSet * bs_union(BitSet const& set1,
                         BitSet const& set2,
                         OUT BitSet & res);
extern BitSet * bs_diff(BitSet const& set1,
                        BitSet const& set2,
                        OUT BitSet & res);
extern BitSet * bs_intersect(BitSet const& set1,
                             BitSet const& set2,
                             OUT BitSet & res);
} //namespace xcom
#endif
