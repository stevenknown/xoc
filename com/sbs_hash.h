/*@
Copyright (c) 2013-2014, Su Zhenyu steven.known@gmail.com

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
#ifndef __SPARSE_BITSET_CORE_HASH_H__
#define __SPARSE_BITSET_CORE_HASH_H__

namespace xcom {

//#define _BIT2NODE_IN_HASH_
#define MD2NODE2_INIT_SZ    8 //The size must be power of 2.

//For given SBitSetCore, mapping MD to its subsequently MD elements via HMap.
template <UINT BitsPerSeg = BITS_PER_SEG>
class Bit2NodeH {
public:
    SBitSetCore<BitsPerSeg> * set; //will be freed by sbs_mgr.
    HMap<UINT, Bit2NodeH*, HashFuncBase2<UINT> > next;

    Bit2NodeH(UINT hash_tab_size = 16) : next(hash_tab_size) { set = NULL; }
    ~Bit2NodeH() {}

    UINT count_mem() const { return next.count_mem() + sizeof(set); }
};


//For given SBitSetCore, mapping bit to its subsequently element via TMap.
template <UINT BitsPerSeg = BITS_PER_SEG>
class Bit2NodeT {
public:
    SBitSetCore<BitsPerSeg> * set; //will be freed by sbs_mgr.
    TMap<UINT, Bit2NodeT*> next;

    Bit2NodeT() { set = NULL; }
    COPY_CONSTRUCTOR(Bit2NodeT);
    ~Bit2NodeT() {}

    void init()
    {
        set = NULL;
        next.init();
    }

    void destroy()
    {
        set = NULL;
        next.destroy();
    }

    size_t count_mem() const { return next.count_mem() + (size_t)sizeof(set); }
};


//Allocator should supply three method: alloc, free, getBsMgr.
//e.g: class Allocator {
//   public:
//   SBitSetCore<BitsPerSeg> * alloc();
//   void free(SBitSetCore<BitsPerSeg>*);
//   MiscBitSetMgr<BitsPerSeg> * getBsMgr() const;
// }
template <class Allocator, UINT BitsPerSeg = BITS_PER_SEG>
class SBitSetCoreHash {
protected:
    SMemPool * m_pool;
    Allocator * m_allocator;

    #ifdef _DEBUG_
    UINT m_num_node; //record the number of MD2Node in the tree.
    #endif

    #ifdef _BIT2NODE_IN_HASH_
    typedef Bit2NodeH<BitsPerSeg> B2NType;
    #else
    typedef Bit2NodeT<BitsPerSeg> B2NType;
    #endif

    B2NType * m_bit2node;

protected:
    inline B2NType * allocB2NType()
    {
        #ifdef _BIT2NODE_IN_HASH_
        B2NType * mn = new B2NType(MD2NODE2_INIT_SZ);
        #else
        B2NType * mn = (B2NType*)smpoolMallocConstSize(sizeof(B2NType), m_pool);
        ::memset(mn, 0, sizeof(B2NType));
        mn->init();
        #endif

        #ifdef _DEBUG_
        m_num_node++;
        #endif

        return mn;
    }

    //dump_helper for SBitSetCore.
    void dump_helper_set(
            FILE * h,
            SBitSetCore<BitsPerSeg> const* set,
            UINT indent,
            UINT id) const
    {
        fprintf(h, "\n");

        UINT i = 0;
        while (i < indent) { fprintf(h, " "); i++; }
        fprintf(h, "%d", id);

        if (set == NULL) { return; }

        fprintf(h, " {");
        SEGIter * iter;
        for (INT j = set->get_first(&iter); j >= 0;) {
            fprintf(h, "%d", j);
            j = set->get_next((UINT)j, &iter);
            if (j >= 0) {
                fprintf(h, ",");
            }
        }
        fprintf(h, "}");
    }

    #ifdef _BIT2NODE_IN_HASH_
    //dump_helper for Bit2NodeH.
    void dump_helper(FILE * h, B2NType * mn, UINT indent) const
    {
        INT pos;
        for (B2NType * nextmn = mn->next.get_first_elem(pos);
             nextmn != NULL; nextmn = mn->next.get_next_elem(pos)) {
            dump_helper_set(h, nextmn->set, indent, (UINT)pos);
            ASSERT0(nextmn);
            dump_helper(h, nextmn, indent + 2);
        }
    }
    #else
    //dump_helper for Bit2NodeT.
    void dump_helper(FILE * h, B2NType * mn, UINT indent) const
    {
        ASSERT0(mn);

        TMapIter<UINT, B2NType*> ti;
        Stack<B2NType*> mn_stack;
        Stack<UINT> indent_stack;
        Stack<UINT> id_stack;

        mn_stack.push(mn);
        id_stack.push(0);
        indent_stack.push(indent);

        for (; mn_stack.get_top() != NULL;) {
            B2NType * mn2 = mn_stack.pop();
            UINT ind = indent_stack.pop();
            UINT id = id_stack.pop();
            ti.clean();
            dump_helper_set(h, mn2->set, ind, id);

            B2NType * nextmn = NULL;
            for (UINT id2 = mn2->next.get_first(ti, &nextmn);
                 id2 != 0; id2 = mn2->next.get_next(ti, &nextmn)) {
                ASSERT0(nextmn);
                mn_stack.push(nextmn);
                indent_stack.push(ind + 2);
                id_stack.push(id2);
            }
        }
    }
    #endif
public:
    SBitSetCoreHash(Allocator * allocator)
    {
        ASSERT0(allocator);
        m_allocator = allocator;
        ASSERTN(allocator, ("Parameter can not be NULL"));
        m_pool = smpoolCreate(sizeof(B2NType) * 4, MEM_CONST_SIZE);

        #ifdef _DEBUG_
        m_num_node = 0;
        #endif

        #ifdef _BIT2NODE_IN_HASH_
        m_bit2node = new B2NType(MD2NODE2_INIT_SZ);
        #else
        m_bit2node = new B2NType();
        #endif
    }
    COPY_CONSTRUCTOR(SBitSetCoreHash);
    virtual ~SBitSetCoreHash()
    {
        destroy();

        #ifdef _BIT2NODE_IN_HASH_
        //m_bit2node already deleted in destroyMN2Node().
        #else
        delete m_bit2node;
        #endif

        smpoolDelete(m_pool);
    }

    void destroy()
    {
        //Destroy the Bit2Node structure, here you do
        //NOT need invoke SBitSetCore's destroy() because they were
        //allocated from set-allocator and will be destroied by
        //the allocator.
        List<B2NType*> wl;

        #ifdef _BIT2NODE_IN_HASH_
        //Do nothing.
        #else
        TMapIter<UINT, B2NType*> ti;
        #endif

        wl.append_tail(get_root());
        while (wl.get_elem_count() != 0) {
            B2NType * mn = wl.remove_head();
            ASSERT0(mn);

            B2NType * nextmn = NULL;

            #ifdef _BIT2NODE_IN_HASH_
            INT pos = 0;
            for (nextmn = mn->next.get_first_elem(pos);
                 nextmn != NULL; nextmn = mn->next.get_next_elem(pos)) {
                wl.append_tail(nextmn);
            }
            #else
            ti.clean();
            for (UINT id = mn->next.get_first(ti, &nextmn);
                 id != 0; id = mn->next.get_next(ti, &nextmn)) {
                ASSERT0(nextmn);
                wl.append_tail(nextmn);
            }
            #endif

            if (mn->set != NULL) {
                m_allocator->free(mn->set);
            }

            mn->next.destroy();

            #ifdef _BIT2NODE_IN_HASH_
            delete mn;
            #else
            //Do nothing.
            #endif
        }
    }

    inline void checkAndGrow(B2NType * mn)
    {
        DUMMYUSE(mn);

        #ifdef _BIT2NODE_IN_HASH_
        //This class use HMap to hash SBitSetCore.
        //As the element is more and more appended,
        //the collisions become more frequently.
        //Extend HMap if the number of element is twice of the hash
        //table size.
        if (mn->next.get_elem_count() > mn->next.get_bucket_size() * 2) {
            mn->next.grow();
        }
        #else
        //Do nothing.
        #endif
    }

    SBitSetCore<BitsPerSeg> const* append(SBitSetCore<BitsPerSeg> const& set)
    {
        SEGIter * iter;
        INT id = set.get_first(&iter);
        if (id < 0) { return NULL; }

        B2NType * mn = m_bit2node->next.get((UINT)id);
        if (mn == NULL) {
            checkAndGrow(m_bit2node);
            mn = allocB2NType();
            m_bit2node->next.set((UINT)id, mn);
        }

        INT nextid = set.get_next((UINT)id, &iter);
        for (; nextid >= 0; nextid = set.get_next((UINT)nextid, &iter)) {
            B2NType * nextmn = mn->next.get((UINT)nextid);
            if (nextmn == NULL) {
                checkAndGrow(mn);
                nextmn = allocB2NType();
                mn->next.set((UINT)nextid, nextmn);
            }
            mn = nextmn;
        }

        ASSERT0(mn);
        if (mn->set == NULL) {
            SBitSetCore<BitsPerSeg> * s = m_allocator->alloc();
            ASSERT0(s);
            s->copy(set, *m_allocator->getBsMgr());
            mn->set = s;
        }
        ASSERT0(mn->set == &set || mn->set->is_equal(set));
        return mn->set;
    }

    size_t count_mem() const
    {
        size_t count = smpoolGetPoolSize(m_pool);
        count += get_root()->count_mem();
        return count;
    }

    //Dump hash tab as tree style.
    void dump(FILE * h) const
    {
        if (h == NULL) { return; }

        fprintf(h, "\n==---- DUMP SBitSetCoreHashHash ----==");

        #ifdef _DEBUG_
        fprintf(h, "\n---- NumOfNode:%d ----",
                m_num_node + 1 /*The first node is m_bit2node*/);
        #endif

        dump_helper(h, get_root(), 1);

        fflush(h);
    }

    //Dump bitset that hashed.
    void dump_hashed_set(FILE * h)
    {
        if (h == NULL) { return; }
        fprintf(h, "\n==---- DUMP SBitSetCoreHashHash ----==");

        List<B2NType*> wl;

        #ifdef _BIT2NODE_IN_HASH_
        //Do nothing.
        #else
        TMapIter<UINT, B2NType*> ti;
        #endif

        wl.append_tail(get_root());
        while (wl.get_elem_count() != 0) {
            B2NType * mn = wl.remove_head();
            ASSERT0(mn);

            B2NType * nextmn = NULL;

            #ifdef _BIT2NODE_IN_HASH_
            INT pos = 0;
            for (nextmn = mn->next.get_first_elem(pos);
                 nextmn != NULL; nextmn = mn->next.get_next_elem(pos)) {
                wl.append_tail(nextmn);
            }
            #else
            ti.clean();
            for (UINT id = mn->next.get_first(ti, &nextmn);
                 id != 0; id = mn->next.get_next(ti, &nextmn)) {
                ASSERT0(nextmn);
                wl.append_tail(nextmn);
            }
            #endif

            if (mn->set != NULL) {
                fprintf(h, "\n");
                mn->set->dump(h);
            }
        }

        fflush(h);
    }

    Allocator * get_allocator() const { return m_allocator; }
    B2NType * get_root() const  { return m_bit2node; }

    //Return the number of SBitSetCore recorded in the hash.
    UINT get_elem_count() const
    {
        UINT count = 0;
        List<B2NType const*> wl;

        #ifdef _BIT2NODE_IN_HASH_
        //Do nothing.
        #else
        TMapIter<UINT, B2NType*> ti;
        #endif

        wl.append_tail(get_root());
        while (wl.get_elem_count() != 0) {
            B2NType const* mn = wl.remove_head();
            ASSERT0(mn);

            B2NType * nextmn = NULL;
            #ifdef _BIT2NODE_IN_HASH_
            INT pos = 0;
            for (nextmn = mn->next.get_first_elem(pos);
                 nextmn != NULL; nextmn = mn->next.get_next_elem(pos)) {
                wl.append_tail(nextmn);
            }
            #else
            ti.clean();
            for (UINT id = mn->next.get_first(ti, &nextmn);
                 id != 0; id = mn->next.get_next(ti, &nextmn)) {
                ASSERT0(nextmn);
                wl.append_tail(nextmn);
            }
            #endif

            if (mn->set != NULL) {
                count++;
            }
        }
        return count;
    }

    //Return true if SBitSetCore pointer has been record in the hash.
    bool find(SBitSetCore<BitsPerSeg> const& set) const
    {
        SEGIter * iter;
        INT id = set.get_first(&iter);
        if (id < 0) { return false; }

        B2NType * mn = get_root()->next.get((UINT)id);
        if (mn == NULL) { return false; }

        INT nextid = set.get_next((UINT)id, &iter);
        for (; nextid >= 0; id = nextid,
             nextid = set.get_next((UINT)nextid, &iter)) {
            B2NType * nextmn = mn->next.get((UINT)nextid);
            if (nextmn == NULL) { return false; }
            mn = nextmn;
        }

        ASSERT0(mn);
        if (mn->set == NULL) {
            return false;
        }
        return mn->set == &set;
    }
};

} //namespace xcom

#endif
