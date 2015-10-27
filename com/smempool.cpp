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
#include "ltype.h"
#include "comf.h"
#include "smempool.h"
#include "sstl.h"

#define msg
#define MAGIC_NUM         0xFC
#define BOUNDARY_NUM      0xAA
#define END_BOUND_BYTE     4

using namespace xcom;

class MEMPOOL_HASH : public Hash<SMemPool*> {
public:
#ifdef _VC6_
    MEMPOOL_HASH():Hash<SMemPool*>(1024){}
#else
    MEMPOOL_HASH():Hash<SMemPool*>::Hash(1024){}
#endif
    ~MEMPOOL_HASH(){}
};


static MEMPOOL_HASH * g_mem_pool_hash_tab = NULL;
static SMemPool * g_mem_pool=NULL;
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
    if (h == NULL) { return; }
    fprintf(h, "\n= SMP, total size:%u ",
            (UINT)smpoolGetPoolSize(handler));
    while (handler != NULL) {
        fprintf(h, "<T%u R%u>",
                (UINT)MEMPOOL_pool_size(handler),
                (UINT)(MEMPOOL_pool_size(handler) -
                       MEMPOOL_start_pos(handler)));
        handler = MEMPOOL_next(handler);
        if (handler != NULL) {
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
    ASSERT(mp, ("create mem pool failed, no enough memory"));
    memset(mp, 0, size_mp);
    memset(((CHAR*)mp) + size_mp + size, BOUNDARY_NUM, END_BOUND_BYTE);

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
    if (t == NULL) return;
    ASSERT(t->prev != NULL, ("t should not be first."));
    t->prev->next = t->next;
    if (t->next != NULL) {
        t->next->prev = t->prev;
    }
    t->next = t->prev = NULL;
}


inline static void append_head_smp(SMemPool ** head, SMemPool * t)
{
    ASSERT(t && head, ("Mem pool internal error 1"));
    t->prev = NULL;
    t->next = *head;
    *head = t;
}


inline static void append_after_smp(SMemPool * marker, SMemPool * tlst)
{
    ASSERT(marker && tlst && marker != tlst, ("Mem pool internal error 2"));
    if (marker->next != NULL) {
        SMemPool * last = tlst;
        while (last != NULL && last->next != NULL) {
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
        while (mp != NULL) {
            //Mainly hash addr of 'mp' into hash
            //table corresponding to 'mpt_idx'.
            ASSERT(g_mem_pool_hash_tab->find((OBJTY)MEMPOOL_id(mp)) == NULL,
                    ("Repetitive pool idx"));
            g_mem_pool_hash_tab->append(mp);
            mp = MEMPOOL_next(mp);
        }

        //Clean up chain info
        mp = g_mem_pool;
        while (g_mem_pool != NULL) {
            mp = g_mem_pool;
            g_mem_pool = g_mem_pool->next;
            mp->prev = mp->next = NULL;
        }
    }
    g_is_pool_init = true;
}


//This function perform finialization works if you invoke the
//smpoolInitPool().
void smpoolFiniPool()
{
    if (g_is_pool_init && g_is_pool_hashed) {
        ASSERT(g_mem_pool == NULL, ("illegal init process"));
        SMemPool * next = NULL;
        INT c;
        for (SMemPool * mp = g_mem_pool_hash_tab->get_first(c);
             mp != NULL; mp = next) {
            next = g_mem_pool_hash_tab->get_next(c);
            g_mem_pool_hash_tab->removed(mp);
            add_next(&g_mem_pool, mp);
        }
        //Code must be placed here! The flag must be reset
        //before the call of pool_hash_tab.destroy().
        g_is_pool_init = false;
        delete g_mem_pool_hash_tab;
        g_mem_pool_hash_tab = NULL;
    }

    g_is_pool_init = false;
    SMemPool * mp = g_mem_pool;
    while (mp != NULL) {
        SMemPool * tmp = mp;
        mp = MEMPOOL_next(mp);
        smpoolDeleteViaPoolIndex(MEMPOOL_id(tmp));
    }
    g_mem_pool = NULL;
    g_mem_pool_count = 0;
}


//Create new mem pool, return the pool handle.
//NOTICE:
//Since this type of pool will NOT to be recorded
//in 'hash table of POOLs', pool index always be 0.
SMemPool * smpoolCreate(size_t size, MEMPOOLTYPE mpt)
{
    SMemPool * mp = NULL;
    if (size == 0 || mpt == MEM_NONE) { return NULL; }
    mp = new_mem_pool(size, mpt);
    return mp;
}


//Create new mem pool, return the pool idx.
#define MAX_TRY 1024
MEMPOOLIDX smpoolCreatePoolIndex(size_t size, MEMPOOLTYPE mpt)
{
    SMemPool * mp = NULL;

    if (size <=0 || mpt == MEM_NONE)
        return 0;

    if (g_is_pool_hashed && g_is_pool_init) {
        MEMPOOLIDX idx,i = 0;
        idx = (MEMPOOLIDX)rand();
        do {
            if (idx != 0 &&
                g_mem_pool_hash_tab->find((OBJTY)(size_t)idx) == NULL) {
                 //Unique pool idx
                break;
            }
            idx = (MEMPOOLIDX)rand();
            i++;
        } while (i < MAX_TRY);

        if (i >= MAX_TRY) {
            ASSERT(0, ("Not any available mempool can be created."));
            return 0;
        }
        mp = smpoolCreate(size, mpt);
        MEMPOOL_id(mp) = idx;
        g_mem_pool_hash_tab->append(mp);
    } else {
        mp = smpoolCreate(size, mpt);
        MEMPOOL_id(mp) = ++g_mem_pool_count;
        if (g_mem_pool == NULL) {
            g_mem_pool = mp;
        } else {
            mp->next = g_mem_pool;
            g_mem_pool->prev = mp;
            g_mem_pool = mp;
        }
    }
    return MEMPOOL_id(mp);
}


//Free mem pool totally.
INT smpoolDelete(SMemPool * handler)
{
    if (handler == NULL) {
        return ST_NO_SUCH_MEMPOOL_FIND;
    }

    //Free local pool list
    SMemPool * tmp = handler;
    while (tmp != NULL) {
        SMemPool * d_tmp = tmp;
        tmp = MEMPOOL_next(tmp);
        free(d_tmp);
    }
    return ST_SUCC;
}


//Free mem pool totally.
INT smpoolDeleteViaPoolIndex(MEMPOOLIDX mpt_idx)
{
    //search the mempool which indicated with 'mpt_idx'
    SMemPool * mp = g_mem_pool;
    if (mpt_idx == MEM_NONE) { return ST_SUCC; }

    //Searching the mempool which indicated with 'mpt_idx'
    if (g_is_pool_hashed && g_is_pool_init) {
        mp = g_mem_pool_hash_tab->find((OBJTY)(size_t)mpt_idx);
        if (mp == NULL) {
            /* Sometimes, mem pool is manipulated by user, but
            is not due to destructer.
            Therefore, the same mem pool idx will be free
            serval times. The message may confuse users.
            fprintf(stdout, "No such mempool, removing failed."); */
              return ST_NO_SUCH_MEMPOOL_FIND;
        }
        //Remove pool from pool table.
        g_mem_pool_hash_tab->removed(mp);
    } else {
        while (mp != NULL) {
            if (MEMPOOL_id(mp) == mpt_idx) {
                break;
            }
            mp = mp->next;
        }

        if (mp == NULL) {
              return ST_NO_SUCH_MEMPOOL_FIND;
        }

        if (mp->prev != NULL) {
              mp->prev->next = mp->next;
        }
        if (mp->next != NULL) {
              mp->next->prev = mp->prev;
        }
        if (mp == g_mem_pool) {
              if (mp->next != NULL) {
                g_mem_pool = mp->next;
              } else {
                g_mem_pool = NULL;
              }
        }
    }

    //Free local pool list
    return smpoolDelete(mp);
}


//Allocate one element from const size pool.
//User must ensure each elment in const size pool are same size.
//'elem_size': indicate the byte size of each element.
void * smpoolMallocConstSize(size_t elem_size, IN SMemPool * handler)
{
    ASSERT(elem_size > 0, ("elem size can not be 0"));
    ASSERT(handler, ("need mempool handler"));
    ASSERT(MEMPOOL_type(handler) == MEM_CONST_SIZE, ("Need const size pool"));
    ASSERT(MEMPOOL_pool_size(handler) >= elem_size &&
            (MEMPOOL_pool_size(handler) % elem_size) == 0,
            ("Pool size must be multiples of element size."));

    //Search free block in the pool.
    ASSERT(MEMPOOL_pool_size(handler) >= MEMPOOL_start_pos(handler),
            ("exception occurs during mempool function"));
    ASSERT(MEMPOOL_pool_size(handler) > 0,
            ("exception occurs during mempool function"));
    size_t s = MEMPOOL_pool_size(handler) - MEMPOOL_start_pos(handler);
    if (elem_size <= s) {
        void * addr = (size_t*)(((BYTE*)MEMPOOL_pool_ptr(handler)) +
                             MEMPOOL_start_pos(handler));
        MEMPOOL_start_pos(handler) += elem_size;
        ASSERT(MEMPOOL_pool_size(handler) >= MEMPOOL_start_pos(handler),
                ("\nexception occurs in handling of pool growing\n"));
        return addr;
    }

    SMemPool * rest = MEMPOOL_next(handler);
    if (rest != NULL) {
        //Search free block in the first rest pool.
        ASSERT(MEMPOOL_pool_size(rest) >= MEMPOOL_start_pos(rest),
                ("exception occurs during mempool function"));
        ASSERT(MEMPOOL_pool_size(rest) > 0,
                ("exception occurs during mempool function"));
        size_t s = MEMPOOL_pool_size(rest) - MEMPOOL_start_pos(rest);
        if (elem_size <= s) {
            void * addr = (size_t*)(((BYTE*)MEMPOOL_pool_ptr(rest)) +
                                 MEMPOOL_start_pos(rest));
            MEMPOOL_start_pos(rest) += elem_size;
            ASSERT(MEMPOOL_pool_size(rest) >= MEMPOOL_start_pos(rest),
                    ("\nexception occurs in handling of pool growing\n"));
            return addr;
        }
    }

    ASSERT(MEMPOOL_grow_size(handler) > 0, ("Mempool's growsize is 0"));

    size_t grow_size = MAX(elem_size * 4, MEMPOOL_grow_size(handler) * 4);
    MEMPOOL_grow_size(handler) = grow_size;
    SMemPool * newpool = new_mem_pool(grow_size, MEM_CONST_SIZE);
    MEMPOOL_prev(newpool) = handler;
    MEMPOOL_next(handler) = newpool;
    MEMPOOL_next(newpool) = rest;
    if (rest != NULL) {
        MEMPOOL_prev(rest) = newpool;
    }
    void * addr = (size_t*)(((CHAR*)MEMPOOL_pool_ptr(newpool)) +
                    MEMPOOL_start_pos(newpool));
    MEMPOOL_start_pos(newpool) += elem_size;
    ASSERT(MEMPOOL_pool_size(newpool) >= MEMPOOL_start_pos(newpool),
            ("\nexception occurs in handling of pool growing\n"));
    return addr;
}


//Query memory space from pool via handler.
//This function will search pool list to find enough memory space to return.
void * smpoolMalloc(size_t size, IN SMemPool * handler, size_t grow_size)
{
    ASSERT(size > 0, ("query size can not be 0"));
    ASSERT(handler, ("need mempool handler"));

    if (size % WORD_ALIGN) {
        size = (size / WORD_ALIGN + 1) * WORD_ALIGN;
    }

    //Search free block in the pool.
    void * addr = NULL;
    SMemPool * tmp_rest = handler, * last = NULL;
    SMemPool * full_head = NULL;
    while (tmp_rest != NULL) {
        ASSERT(MEMPOOL_pool_size(tmp_rest) >= MEMPOOL_start_pos(tmp_rest),
                ("exception occurs during mempool function"));
        ASSERT(MEMPOOL_pool_size(tmp_rest) > 0,
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
    ASSERT(last && MEMPOOL_next(last) == NULL, ("Mempool internal error 3"));

    //We lack free blocks, and quering new mem block.
    tmp_rest = last;
    if (grow_size == 0) {
        ASSERT(MEMPOOL_grow_size(handler), ("grow size is 0"));
        grow_size = MEMPOOL_grow_size(handler) * 4;
        MEMPOOL_grow_size(handler) = grow_size;
    }

    if (size > grow_size) {
        MEMPOOL_next(tmp_rest) = new_mem_pool(
                            (size / grow_size + 1) * grow_size,
                            MEM_COMM);
    } else {
        MEMPOOL_next(tmp_rest) = new_mem_pool(grow_size, MEM_COMM);
    }

    MEMPOOL_prev(MEMPOOL_next(tmp_rest)) = tmp_rest;
    tmp_rest = MEMPOOL_next(tmp_rest);
    addr = (size_t*)(((CHAR*)MEMPOOL_pool_ptr(tmp_rest)) +
                    MEMPOOL_start_pos(tmp_rest));
FIN:
    if (full_head != NULL) {
        append_after_smp(tmp_rest, full_head);
    }
    MEMPOOL_start_pos(tmp_rest) += size;
    ASSERT(MEMPOOL_pool_size(tmp_rest) >= MEMPOOL_start_pos(tmp_rest),
            ("\nexception occurs in handling of pool growing\n"));
    return addr;
}


//Quering memory space from pool via pool index.
void * smpoolMallocViaPoolIndex(size_t size, MEMPOOLIDX mpt_idx, size_t grow_size)
{
    ASSERT(size > 0, ("Request size can not be 0"));
    SMemPool * mp = g_mem_pool;

    //Searching the mempool which indicated with 'mpt_idx'
    if (g_is_pool_hashed && g_is_pool_init) {
        mp = g_mem_pool_hash_tab->find((OBJTY)(size_t)mpt_idx);
    } else {
        while (mp != NULL) {
            if (MEMPOOL_id(mp) == mpt_idx) {
                break;
            }
            mp = mp->next;
        }
    }

    if (mp == NULL) {
        //Mem pool of Index %lu does not exist", (ULONG)mpt_idx);
        return NULL;
    }

    return smpoolMalloc(size, mp, grow_size);
}


//Get total pool byte-size.
size_t smpoolGetPoolSize(SMemPool const* handle)
{
    if (handle == NULL) return 0;
    SMemPool const* mp = handle;
    size_t size = 0;
    while (mp != NULL) {
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
        mp = g_mem_pool_hash_tab->find((OBJTY)(size_t)mpt_idx);
    } else {
        //Searching the mempool which indicated with 'mpt_idx'
        while (mp!=NULL) {
            if(MEMPOOL_id(mp) == mpt_idx ){
                break;
            }
            mp = mp->next;
        }
    }

    if (mp == NULL) {
        return 0;
    }

    //Searching free mem block in the mempool
    //which 'mpt_idx' refers to.
    return smpoolGetPoolSize(mp);
}

