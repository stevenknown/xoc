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
#include "xcominc.h"

#define msg
#define MAGIC_NUM 0xFC
#define BOUNDARY_NUM 0xAA
#define END_BOUND_BYTE 4

using xcom::Hash;

class MEMPOOL_HASH : public Hash<SMemPool*> {
public:
#ifdef _VC6_
    MEMPOOL_HASH():Hash<SMemPool*>(1024){}
#else
    MEMPOOL_HASH():Hash<SMemPool*>::Hash(1024){}
#endif
    ~MEMPOOL_HASH(){}
};


static MEMPOOL_HASH * g_mem_pool_hash_tab = nullptr;
static SMemPool * g_mem_pool=nullptr;
static UINT g_mem_pool_count = 0;
#ifdef _DEBUG_
static UINT g_mem_pool_chunk_count = 0;
#endif


//First of all marker of memory pool should be initialized
static bool g_is_pool_init = false;

//Build hash table of memory pool
static bool g_is_pool_hashed = true;
ULONGLONG g_stat_mem_size = 0;


void dumpPool(SMemPool * handler, FILE * h)
{
    if (h == nullptr) { return; }
    fprintf(h, "\n= SMP, total size:%u ",
            (UINT)smpoolGetPoolSize(handler));
    while (handler != nullptr) {
        fprintf(h, "<T%u R%u>",
                (UINT)MEMPOOL_pool_size(handler),
                (UINT)(MEMPOOL_pool_size(handler) -
                       MEMPOOL_start_pos(handler)));
        handler = MEMPOOL_next(handler);
        if (handler != nullptr) {
            fprintf(h, ", ");
        }
    }
    fflush(h);
}


static SMemPool * new_mem_pool(size_t size, MEMPOOLTYPE mpt)
{
    INT size_mp = sizeof(SMemPool);
    if (size_mp % WORD_ALIGN) {
        size_mp = (sizeof(SMemPool) / WORD_ALIGN + 1 ) * WORD_ALIGN;
    }

    SMemPool * mp = (SMemPool*)malloc(size_mp + size + END_BOUND_BYTE);
    ASSERTN(mp, ("create mem pool failed, no enough memory"));
    ::memset(mp, 0, size_mp);
    ::memset(((CHAR*)mp) + size_mp + size, BOUNDARY_NUM, END_BOUND_BYTE);

    MEMPOOL_type(mp) = mpt;
    #ifdef _DEBUG_
    g_stat_mem_size += size_mp + size; //Only for statistic purpose
    MEMPOOL_chunk_id(mp) = ++g_mem_pool_chunk_count;
    #endif
    MEMPOOL_pool_ptr(mp) = ((CHAR*)mp) + size_mp;
    MEMPOOL_pool_size(mp) = size;
    MEMPOOL_start_pos(mp) = 0;
    MEMPOOL_grow_size(mp) = size;
    return mp;
}


inline static void remove_smp(SMemPool * t)
{
    if (t == nullptr) return;
    ASSERTN(t->prev != nullptr, ("t should not be first."));
    t->prev->next = t->next;
    if (t->next != nullptr) {
        t->next->prev = t->prev;
    }
    t->next = t->prev = nullptr;
}


inline static void append_head_smp(SMemPool ** head, SMemPool * t)
{
    ASSERTN(t && head, ("Mem pool internal error 1"));
    t->prev = nullptr;
    t->next = *head;
    *head = t;
}


inline static void append_after_smp(SMemPool * marker, SMemPool * tlst)
{
    ASSERTN(marker && tlst && marker != tlst, ("Mem pool internal error 2"));
    if (marker->next != nullptr) {
        SMemPool * last = tlst;
        while (last != nullptr && last->next != nullptr) {
            last = last->next;
        }
        marker->next->prev = last;
        last->next = marker->next;
    }
    tlst->prev = marker;
    marker->next = tlst;
}


//This function do some initializations if you want to manipulate pool
//via pool index.
//Note if you just create pool and manipulate pool via handler,
//the initialization is dispensable.
//Hash table must be initialized if one invoked
//smpoolCreatePoolIndex or smpoolMalloc.
void smpoolInitPool()
{
    if (g_is_pool_init) {
        return;
    }

    if (g_is_pool_hashed) {
        g_mem_pool_hash_tab = new MEMPOOL_HASH();
        SMemPool * mp = g_mem_pool;

        //Record pool list into hash.
        while (mp != nullptr) {
            //Mainly hash addr of 'mp' into hash
            //table corresponding to 'mpt_idx'.
            ASSERTN(g_mem_pool_hash_tab->find(
                (xcom::OBJTY)MEMPOOL_id(mp)) == nullptr,
                ("Repetitive pool idx"));
            g_mem_pool_hash_tab->append(mp);
            mp = MEMPOOL_next(mp);
        }

        //Clean up chain info
        mp = g_mem_pool;
        while (g_mem_pool != nullptr) {
            mp = g_mem_pool;
            g_mem_pool = g_mem_pool->next;
            mp->prev = mp->next = nullptr;
        }
    }
    g_is_pool_init = true;
}


//This function perform finialization works
//if smpoolInitPool() has been invoked.
void smpoolFiniPool()
{
    if (g_is_pool_init && g_is_pool_hashed) {
        ASSERTN(g_mem_pool == nullptr, ("illegal init process"));
        SMemPool * next = nullptr;
        INT c;
        for (SMemPool * mp = g_mem_pool_hash_tab->get_first(c);
             mp != nullptr; mp = next) {
            next = g_mem_pool_hash_tab->get_next(c);
            g_mem_pool_hash_tab->remove(mp);
            xcom::add_next(&g_mem_pool, mp);
        }
        //Code must be placed here! The flag must be reset
        //before the call of pool_hash_tab.destroy().
        g_is_pool_init = false;
        delete g_mem_pool_hash_tab;
        g_mem_pool_hash_tab = nullptr;
    }

    g_is_pool_init = false;
    SMemPool * mp = g_mem_pool;
    while (mp != nullptr) {
        SMemPool * tmp = mp;
        mp = MEMPOOL_next(mp);
        smpoolDeleteViaPoolIndex(MEMPOOL_id(tmp));
    }
    g_mem_pool = nullptr;
    g_mem_pool_count = 0;
}


//Create new memory pool, return the pool handle.
//size: the initial byte size of pool. For MEM_CONST_SIZE, 'size'
//      must be integer multiples of element byte size.
//mpt: pool type.
SMemPool * smpoolCreate(size_t size, MEMPOOLTYPE mpt)
{
    SMemPool * mp = nullptr;
    if (size == 0 || mpt == MEM_NONE) { return nullptr; }
    mp = new_mem_pool(size, mpt);
    return mp;
}


//Create new memory pool, return the pool idx.
#define MAX_TRY 1024
//size: the initial byte size of pool. For MEM_CONST_SIZE, 'size'
//      must be integer multiples of element byte size.
//mpt: pool type.
MEMPOOLIDX smpoolCreatePoolIndex(size_t size, MEMPOOLTYPE mpt)
{
    SMemPool * mp = nullptr;

    if (size <=0 || mpt == MEM_NONE)
        return 0;

    if (g_is_pool_hashed && g_is_pool_init) {
        MEMPOOLIDX idx,i = 0;
        idx = (MEMPOOLIDX)rand();
        do {
            if (idx != 0 &&
                g_mem_pool_hash_tab->find((xcom::OBJTY)(size_t)idx) == nullptr) {
                 //Unique pool idx
                break;
            }
            idx = (MEMPOOLIDX)::rand();
            i++;
        } while (i < MAX_TRY);

        if (i >= MAX_TRY) {
            ASSERTN(0, ("Not any available mempool can be created."));
            return 0;
        }
        mp = smpoolCreate(size, mpt);
        MEMPOOL_id(mp) = idx;
        g_mem_pool_hash_tab->append(mp);
    } else {
        mp = smpoolCreate(size, mpt);
        MEMPOOL_id(mp) = ++g_mem_pool_count;
        if (g_mem_pool == nullptr) {
            g_mem_pool = mp;
        } else {
            mp->next = g_mem_pool;
            g_mem_pool->prev = mp;
            g_mem_pool = mp;
        }
    }
    return MEMPOOL_id(mp);
}


//Destroy memory pool totally.
INT smpoolDelete(SMemPool * handler)
{
    if (handler == nullptr) {
        return ST_NO_SUCH_MEMPOOL_FIND;
    }

    //Free local pool list
    SMemPool * tmp = handler;
    while (tmp != nullptr) {
        SMemPool * d_tmp = tmp;
        tmp = MEMPOOL_next(tmp);
        free(d_tmp);
    }
    return ST_SUCC;
}


//Destroy mem pool totally.
INT smpoolDeleteViaPoolIndex(MEMPOOLIDX mpt_idx)
{
    //search the mempool which indicated with 'mpt_idx'
    SMemPool * mp = g_mem_pool;
    if (mpt_idx == MEM_NONE) { return ST_SUCC; }

    //Searching the mempool which indicated with 'mpt_idx'
    if (g_is_pool_hashed && g_is_pool_init) {
        mp = g_mem_pool_hash_tab->find((xcom::OBJTY)(size_t)mpt_idx);
        if (mp == nullptr) {
            //Sometimes, mem pool is manipulated by user, but
            //is not due to destructer.
            //Therefore, the same mem pool idx will be free
            //serval times. The message may confuse users.
            //fprintf(stdout, "No such mempool, removing failed.");
            return ST_NO_SUCH_MEMPOOL_FIND;
        }
        //Remove pool from pool table.
        g_mem_pool_hash_tab->remove(mp);
    } else {
        while (mp != nullptr) {
            if (MEMPOOL_id(mp) == mpt_idx) {
                break;
            }
            mp = mp->next;
        }

        if (mp == nullptr) {
            return ST_NO_SUCH_MEMPOOL_FIND;
        }

        if (mp->prev != nullptr) {
            mp->prev->next = mp->next;
        }
        if (mp->next != nullptr) {
            mp->next->prev = mp->prev;
        }
        if (mp == g_mem_pool) {
            if (mp->next != nullptr) {
                g_mem_pool = mp->next;
            } else {
                g_mem_pool = nullptr;
            }
        }
    }

    //Free local pool list
    return smpoolDelete(mp);
}


//Allocate one element from const size pool.
//User must ensure each element in const size pool are same size.
//elem_size: the byte size of each element.
void * smpoolMallocConstSize(size_t elem_size, IN SMemPool * handler)
{
    ASSERTN(elem_size > 0, ("elem size can not be 0"));
    ASSERTN(handler, ("need mempool handler"));
    ASSERTN(MEMPOOL_type(handler) == MEM_CONST_SIZE, ("Need const size pool"));
    ASSERTN(MEMPOOL_pool_size(handler) >= elem_size &&
        (MEMPOOL_pool_size(handler) % elem_size) == 0,
        ("Pool size must be multiples of element size."));

    //Search free block in the pool.
    ASSERTN(MEMPOOL_pool_size(handler) >= MEMPOOL_start_pos(handler),
        ("exception occurs during mempool function"));
    ASSERTN(MEMPOOL_pool_size(handler) > 0,
        ("exception occurs during mempool function"));
    size_t s = MEMPOOL_pool_size(handler) - MEMPOOL_start_pos(handler);
    if (elem_size <= s) {
        void * addr = (size_t*)(((BYTE*)MEMPOOL_pool_ptr(handler)) +
            MEMPOOL_start_pos(handler));
        MEMPOOL_start_pos(handler) += elem_size;
        ASSERTN(MEMPOOL_pool_size(handler) >= MEMPOOL_start_pos(handler),
            ("\nexception occurs in handling of pool growing\n"));
        return addr;
    }

    SMemPool * rest = MEMPOOL_next(handler);
    if (rest != nullptr) {
        //Search free block in the first rest pool.
        ASSERTN(MEMPOOL_pool_size(rest) >= MEMPOOL_start_pos(rest),
            ("exception occurs during mempool function"));
        ASSERTN(MEMPOOL_pool_size(rest) > 0,
            ("exception occurs during mempool function"));
        size_t s2 = MEMPOOL_pool_size(rest) - MEMPOOL_start_pos(rest);
        if (elem_size <= s2) {
            void * addr = (size_t*)(((BYTE*)MEMPOOL_pool_ptr(rest)) +
                MEMPOOL_start_pos(rest));
            MEMPOOL_start_pos(rest) += elem_size;
            ASSERTN(MEMPOOL_pool_size(rest) >= MEMPOOL_start_pos(rest),
                ("\nexception occurs in handling of pool growing\n"));
            return addr;
        }
    }

    ASSERTN(MEMPOOL_grow_size(handler) > 0, ("Mempool's growsize is 0"));

    size_t grow_size = MAX(elem_size * 4, MEMPOOL_grow_size(handler) * 4);
    MEMPOOL_grow_size(handler) = grow_size;
    SMemPool * newpool = new_mem_pool(grow_size, MEM_CONST_SIZE);
    MEMPOOL_prev(newpool) = handler;
    MEMPOOL_next(handler) = newpool;
    MEMPOOL_next(newpool) = rest;
    if (rest != nullptr) {
        MEMPOOL_prev(rest) = newpool;
    }
    void * addr = (size_t*)(((CHAR*)MEMPOOL_pool_ptr(newpool)) +
        MEMPOOL_start_pos(newpool));
    MEMPOOL_start_pos(newpool) += elem_size;
    ASSERTN(MEMPOOL_pool_size(newpool) >= MEMPOOL_start_pos(newpool),
        ("\nexception occurs in handling of pool growing\n"));
    return addr;
}


//Query memory space from pool via handler.
//This function will search pool list to find enough memory space to return.
void * smpoolMalloc(size_t size, IN SMemPool * handler, size_t grow_size)
{
    ASSERTN(size > 0, ("query size can not be 0"));
    ASSERTN(handler, ("need mempool handler"));

    if (size % WORD_ALIGN) {
        size = (size / WORD_ALIGN + 1) * WORD_ALIGN;
    }

    //Search free block in the pool.
    void * addr = nullptr;
    SMemPool * tmp_rest = handler, * last = nullptr;
    SMemPool * full_head = nullptr;
    while (tmp_rest != nullptr) {
        ASSERTN(MEMPOOL_pool_size(tmp_rest) >= MEMPOOL_start_pos(tmp_rest),
            ("exception occurs during mempool function"));
        ASSERTN(MEMPOOL_pool_size(tmp_rest) > 0,
            ("exception occurs during mempool function"));
        size_t s = MEMPOOL_pool_size(tmp_rest) - MEMPOOL_start_pos(tmp_rest);
        if (size <= s) {
            addr = (size_t*)(((BYTE*)MEMPOOL_pool_ptr(tmp_rest)) +
                MEMPOOL_start_pos(tmp_rest));
            goto FIN;
        }

        SMemPool * cur = tmp_rest;
        tmp_rest = MEMPOOL_next(tmp_rest);
        if (s <= MIN_MARGIN && cur != handler) {
            remove_smp(cur);
            append_head_smp(&full_head, cur);
        } else {
            last = cur;
        }
    }
    ASSERTN(last && MEMPOOL_next(last) == nullptr, ("Mempool internal error 3"));

    //We lack free blocks, and quering new mem block.
    tmp_rest = last;
    if (grow_size == 0) {
        ASSERTN(MEMPOOL_grow_size(handler), ("grow size is 0"));
        grow_size = MEMPOOL_grow_size(handler) * 4;
        MEMPOOL_grow_size(handler) = grow_size;
    }

    if (size > grow_size) {
        MEMPOOL_next(tmp_rest) = new_mem_pool(
            (size / grow_size + 1) * grow_size, MEM_COMM);
    } else {
        MEMPOOL_next(tmp_rest) = new_mem_pool(grow_size, MEM_COMM);
    }

    MEMPOOL_prev(MEMPOOL_next(tmp_rest)) = tmp_rest;
    tmp_rest = MEMPOOL_next(tmp_rest);
    addr = (size_t*)(((CHAR*)MEMPOOL_pool_ptr(tmp_rest)) +
        MEMPOOL_start_pos(tmp_rest));
FIN:
    if (full_head != nullptr) {
        append_after_smp(tmp_rest, full_head);
    }
    MEMPOOL_start_pos(tmp_rest) += size;
    ASSERTN(MEMPOOL_pool_size(tmp_rest) >= MEMPOOL_start_pos(tmp_rest),
        ("\nexception occurs in handling of pool growing\n"));
    return addr;
}


//Quering memory space from pool via pool index.
void * smpoolMallocViaPoolIndex(size_t size, MEMPOOLIDX mpt_idx, size_t grow_size)
{
    ASSERTN(size > 0, ("Request size can not be 0"));
    SMemPool * mp = g_mem_pool;

    //Searching the mempool which indicated with 'mpt_idx'
    if (g_is_pool_hashed && g_is_pool_init) {
        mp = g_mem_pool_hash_tab->find((xcom::OBJTY)(size_t)mpt_idx);
    } else {
        while (mp != nullptr) {
            if (MEMPOOL_id(mp) == mpt_idx) {
                break;
            }
            mp = mp->next;
        }
    }

    if (mp == nullptr) {
        //Mem pool of Index %lu does not exist", (ULONG)mpt_idx);
        return nullptr;
    }

    return smpoolMalloc(size, mp, grow_size);
}


//Get total pool byte-size.
size_t smpoolGetPoolSize(SMemPool const* handle)
{
    if (handle == nullptr) return 0;
    SMemPool const* mp = handle;
    size_t size = 0;
    while (mp != nullptr) {
        size += mp->mem_pool_size;
        mp = MEMPOOL_next(mp);
    }
    return size;
}


//Get total pool byte-size.
size_t smpoolGetPoolSizeViaIndex(MEMPOOLIDX mpt_idx)
{
    SMemPool * mp = g_mem_pool;

    if (g_is_pool_hashed && g_is_pool_init) {
        mp = g_mem_pool_hash_tab->find((xcom::OBJTY)(size_t)mpt_idx);
    } else {
        //Searching the mempool which indicated with 'mpt_idx'
        while (mp!=nullptr) {
            if(MEMPOOL_id(mp) == mpt_idx ){
                break;
            }
            mp = mp->next;
        }
    }

    if (mp == nullptr) {
        return 0;
    }

    //Searching free mem block in the mempool
    //which 'mpt_idx' refers to.
    return smpoolGetPoolSize(mp);
}

