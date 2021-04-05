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

//Consider the speed of TMap might be better than HMap on average.
//#define _BIT2NODE_IN_HASH_
#define MD2NODE2_INIT_SZ 8 //The size must be power of 2.

//For given SBitSetCore, mapping MD to its subsequently MD elements via HMap.
template <UINT BitsPerSeg = BITS_PER_SEG>
class Bit2NodeH {
public:
    SBitSetCore<BitsPerSeg> * set; //will be freed by sbs_mgr.
    HMap<UINT, Bit2NodeH*, HashFuncBase2<UINT> > next;
public:
    Bit2NodeH(UINT hash_tab_size = 16) : next(hash_tab_size) { set = nullptr; }
    ~Bit2NodeH() {}
    //Count memory usage for current object.
    size_t count_mem() const { return next.count_mem() + sizeof(set); }
};


//For given SBitSetCore, mapping bit to its subsequently element via TMap.
template <UINT BitsPerSeg = BITS_PER_SEG>
class Bit2NodeT {
    COPY_CONSTRUCTOR(Bit2NodeT);
public:
    SBitSetCore<BitsPerSeg> * set; //will be freed by sbs_mgr.
    TMap<UINT, Bit2NodeT*> next;
public:
    Bit2NodeT(SMemPool * pool = nullptr) : next(pool) { set = nullptr; }
    ~Bit2NodeT() {}
    void init(SMemPool * pool = nullptr)
    {
        set = nullptr;
        next.init(pool);
    }
    void destroy()
    {
        set = nullptr;
        next.destroy();
    }
    //Count memory usage for current object.
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
    COPY_CONSTRUCTOR(SBitSetCoreHash);
protected:
    SMemPool * m_pool;
    Allocator * m_allocator;
    List<SBitSetCore<BitsPerSeg>*> m_bit2node_set_list;

    #ifdef _DEBUG_
    UINT m_num_node; //record the number of MD2Node in the tree.
    #endif

    #ifdef _BIT2NODE_IN_HASH_
    typedef Bit2NodeH<BitsPerSeg> B2NType;
    #else
    typedef Bit2NodeT<BitsPerSeg> B2NType;
    SMemPool * m_rbtn_pool; //pool to store RBTNType.
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
        mn->init(m_rbtn_pool);
        #endif

        #ifdef _DEBUG_
        m_num_node++;
        #endif

        return mn;
    }

    //dump_helper for SBitSetCore.
    void dump_helper_set(FILE * h,
                         SBitSetCore<BitsPerSeg> const* set,
                         UINT indent,
                         UINT id) const
    {
        fprintf(h, "\n");
        UINT i = 0;
        while (i < indent) { fprintf(h, " "); i++; }
        fprintf(h, "%d", id);
        if (set == nullptr) { return; }
        fprintf(h, " {");
        SEGIter * iter = nullptr;
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
             nextmn != nullptr; nextmn = mn->next.get_next_elem(pos)) {
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

        for (; mn_stack.get_top() != nullptr;) {
            B2NType * mn2 = mn_stack.pop();
            UINT ind = indent_stack.pop();
            UINT id = id_stack.pop();
            ti.clean();
            dump_helper_set(h, mn2->set, ind, id);

            B2NType * nextmn = nullptr;
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
        ASSERTN(allocator, ("Parameter can not be nullptr"));
        m_pool = smpoolCreate(sizeof(B2NType) * 4, MEM_CONST_SIZE);

        #ifdef _DEBUG_
        m_num_node = 0;
        #endif

        #ifdef _BIT2NODE_IN_HASH_
        m_bit2node = new B2NType(MD2NODE2_INIT_SZ);
        #else
        m_rbtn_pool = smpoolCreate(
            sizeof(RBTNode<UINT, Bit2NodeT<BitsPerSeg>*>) * 4,
            MEM_CONST_SIZE);
        m_bit2node = new B2NType();
        #endif
    }
    virtual ~SBitSetCoreHash()
    {
        destroy();

        #ifdef _BIT2NODE_IN_HASH_
        //Nothing to do.
        //Do not detete m_bit2node, it has already been deleted in destroy().
        #else
        smpoolDelete(m_rbtn_pool);
        delete m_bit2node;
        #endif

        m_bit2node = nullptr;
        smpoolDelete(m_pool);
    }

    #ifdef _BIT2NODE_IN_HASH_
    void destroyBit2NodeH()
    {
        //Destroy the Bit2Node structure, here you do
        //NOT need invoke SBitSetCore's destroy() because they were
        //allocated from set-allocator and will be destroied by
        //the allocator.
        List<B2NType*> wl;
        wl.append_tail(get_root());
        while (wl.get_elem_count() != 0) {
            B2NType * mn = wl.remove_head();
            ASSERT0(mn);
            B2NType * nextmn = nullptr;
            INT pos = 0;
            for (nextmn = mn->next.get_first_elem(pos);
                 nextmn != nullptr; nextmn = mn->next.get_next_elem(pos)) {
                wl.append_tail(nextmn);
            }
            if (mn->set != nullptr) {
                m_allocator->free(mn->set);
            }
            mn->next.destroy();
            delete mn;
        }
    }
    #else
    void destroyBit2NodeT()
    {

        //Destroy the Bit2Node structure, here you do
        //NOT need invoke SBitSetCore's destroy() because they were
        //allocated from set-allocator and will be destroied by
        //the allocator.

        //The iteration is very slow if there are too many Bit2NodeTs.
        //#define ITER_EACH_BIT2NODE_TO_FREE_SET
        #ifdef ITER_EACH_BIT2NODE_TO_FREE_SET
        List<B2NType*> wl;
        TMapIter<UINT, B2NType*> ti;
        wl.append_tail(get_root());
        while (wl.get_elem_count() != 0) {
            B2NType * mn = wl.remove_head();
            ASSERT0(mn);
            B2NType * nextmn = nullptr;
            ti.clean();
            for (UINT id = mn->next.get_first(ti, &nextmn);
                 id != 0; id = mn->next.get_next(ti, &nextmn)) {
                ASSERT0(nextmn);
                wl.append_tail(nextmn);
            }
            if (mn->set != nullptr) {
                m_allocator->free(mn->set);
            }
            mn->next.destroy();
        }
        #else
        C<SBitSetCore<BitsPerSeg>*> * ct = nullptr;
        for (SBitSetCore<BitsPerSeg>* set = m_bit2node_set_list.get_head(&ct);
             ct != m_bit2node_set_list.end();
             set = m_bit2node_set_list.get_next(&ct)) {
            ASSERT0(set);
            m_allocator->free(set);
        }
        #endif
    }
    #endif

    void destroy()
    {
        #ifdef _BIT2NODE_IN_HASH_
        destroyBit2NodeH();
        #else
        destroyBit2NodeT();
        #endif
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
        //Nothing to do.
        #endif
    }

    SBitSetCore<BitsPerSeg> const* append(SBitSetCore<BitsPerSeg> const& set)
    {
        SEGIter * iter = nullptr;
        INT id = set.get_first(&iter);
        if (id < 0) { return nullptr; }

        B2NType * mn = m_bit2node->next.get((UINT)id);
        if (mn == nullptr) {
            checkAndGrow(m_bit2node);
            mn = allocB2NType();
            m_bit2node->next.set((UINT)id, mn);
        }

        INT nextid = set.get_next((UINT)id, &iter);
        for (; nextid >= 0; nextid = set.get_next((UINT)nextid, &iter)) {
            B2NType * nextmn = mn->next.get((UINT)nextid);
            if (nextmn == nullptr) {
                checkAndGrow(mn);
                nextmn = allocB2NType();
                mn->next.set((UINT)nextid, nextmn);
            }
            mn = nextmn;
        }

        ASSERT0(mn);
        if (mn->set == nullptr) {
            SBitSetCore<BitsPerSeg> * s = m_allocator->alloc();
            ASSERT0(s);
            s->copy(set, *m_allocator->getBsMgr());
            mn->set = s;
            m_bit2node_set_list.append_tail(s);
        }
        ASSERT0(mn->set == &set || mn->set->is_equal(set));
        return mn->set;
    }
    //Count memory usage for current object.
    size_t count_mem() const
    {
        size_t count = smpoolGetPoolSize(m_pool);
        #ifdef _BIT2NODE_IN_HASH_
        //Nothing to do.
        #else
        count += smpoolGetPoolSize(m_rbtn_pool);
        #endif
        count += get_root()->count_mem();
        return count;
    }

    //Dump hash tab as tree style.
    void dump(FILE * h) const
    {
        if (h == nullptr) { return; }
        fprintf(h, "\n==---- DUMP SBitSetCoreHashHash ----==");

        #ifdef _DEBUG_
        fprintf(h, "\n---- NumOfNode:%d ----",
                m_num_node + 1 /*The first node is m_bit2node*/);
        #endif

        dump_helper(h, get_root(), 1);
        fflush(h);
    }

    //Dump bitset that hashed.
    void dump_hashed_set(FILE * h) const
    {
        if (h == nullptr) { return; }
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

            B2NType * nextmn = nullptr;

            #ifdef _BIT2NODE_IN_HASH_
            INT pos = 0;
            for (nextmn = mn->next.get_first_elem(pos);
                 nextmn != nullptr; nextmn = mn->next.get_next_elem(pos)) {
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

            if (mn->set != nullptr) {
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

            B2NType * nextmn = nullptr;
            #ifdef _BIT2NODE_IN_HASH_
            INT pos = 0;
            for (nextmn = mn->next.get_first_elem(pos);
                 nextmn != nullptr; nextmn = mn->next.get_next_elem(pos)) {
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

            if (mn->set != nullptr) {
                count++;
            }
        }
        return count;
    }

    //Return true if SBitSetCore pointer has been record in the hash.
    bool find(SBitSetCore<BitsPerSeg> const& set) const
    {
        SEGIter * iter = nullptr;
        INT id = set.get_first(&iter);
        if (id < 0) { return false; }

        B2NType * mn = get_root()->next.get((UINT)id);
        if (mn == nullptr) { return false; }

        INT nextid = set.get_next((UINT)id, &iter);
        for (; nextid >= 0; id = nextid,
             nextid = set.get_next((UINT)nextid, &iter)) {
            B2NType * nextmn = mn->next.get((UINT)nextid);
            if (nextmn == nullptr) { return false; }
            mn = nextmn;
        }

        ASSERT0(mn);
        if (mn->set == nullptr) {
            return false;
        }
        return mn->set == &set;
    }
};

} //namespace xcom

#endif
