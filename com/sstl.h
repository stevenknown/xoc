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
#ifndef __SSTL_H__
#define __SSTL_H__

namespace xcom {
template <class T> class allocator {
public:
    allocator() throw() {}
    allocator (allocator const& alloc) throw() {}
    template <class U> allocator (allocator<U> const& alloc) throw() {}
    ~allocator() {}
};
} //namespace xcom


template <class T>
void * operator new(size_t size, xcom::allocator<T> * pool)
{
    UNUSED(pool);
    return ::operator new(size);
}


template <class T>
void operator delete(void * ptr, xcom::allocator<T> * pool)
{
    UNUSED(pool);
    ::operator delete(ptr);
}

namespace xcom {

#define NO_BASIC_MAT_DUMP //Default option
#define MAX_SHASH_BUCKET 97 //Default option

typedef void* OBJTY;

//Structure chain operations.
//For easing implementation, there must be 2 fields declared in T
//    1. T * next
//    2. T * prev
template <class T>
inline UINT find_position(T const* list, T const* t)
{
    for (UINT c = 0; list != NULL; c++, list = list->next) {
        if (list == t) { return c; }
    }
    UNREACH(); //not find.
    return 0;
}


template <class T>
inline UINT cnt_list(T const* t)
{
    UINT c = 0;
    while (t != NULL) { c++; t = t->next; }
    return c;
}


//Return true if p is in current list.
template <class T>
bool in_list(T const* head, T const* p)
{
    if (p == NULL) { return true; }
    T const* t = head;
    while (t != NULL) {
        if (t == p) { return true; }
        t = t->next;
    }
    return false;
}


template <class T>
inline T * get_last(T * t)
{
    while (t != NULL && t->next != NULL) { t = t->next; }
    return t;
}


template <class T>
inline T * get_first(T * t)
{
    while (t != NULL && t->prev != NULL) { t = t->prev; }
    return t;
}


template <class T>
inline void add_next(T ** pheader, T * t)
{
    if (pheader == NULL || t == NULL) return ;
    T * p = NULL;
    t->prev = NULL;
    if ((*pheader) == NULL) {
        *pheader = t;
    } else {
        p = (*pheader)->next;
        ASSERT(t != *pheader, ("\n<add_next> : overlap list member\n"));
        if (p == NULL) {
            (*pheader)->next = t;
            t->prev = *pheader;
        } else {
            while (p->next != NULL) {
                p = p->next;
                ASSERT(p != t, ("\n<add_next> : overlap list member\n"));
            }
            p->next = t;
            t->prev = p;
        }
    }
}


template <class T>
inline void add_next(IN OUT T ** pheader, IN OUT T ** last, IN T * t)
{
    if (pheader == NULL || t == NULL) { return ; }

    t->prev = NULL;
    if ((*pheader) == NULL) {
        *pheader = t;
        while (t->next != NULL) { t = t->next; }
        *last = t;
    } else {
        ASSERT0(last != NULL && *last != NULL && (*last)->next == NULL);
        (*last)->next = t;
        t->prev = *last;
        while (t->next != NULL) { t = t->next; }
        *last = t;
    }
}


template <class T>
inline void remove(T ** pheader, T * t)
{
    if (pheader == NULL || t == NULL) { return; }
    if (t == *pheader) {
        *pheader = t->next;
        if (*pheader != NULL) {
            (*pheader)->prev = NULL;
        }
        t->next = t->prev = NULL;
        return;
    }

    ASSERT(t->prev, ("t is not in list"));
    t->prev->next = t->next;
    if (t->next != NULL) {
        t->next->prev = t->prev;
    }
    t->next = t->prev = NULL;
}


template <class T>
inline void replace(T ** pheader, T * olds, T * news)
{
    if (pheader == NULL || olds == NULL) return;
    if (olds == news) { return; }
    if (news == NULL) {
        xcom::remove(pheader, olds);
        return;
    }

    #ifdef _DEBUG_
    bool find = false;
    T * p = *pheader;
    while (p != NULL) {
        if (p == olds) {
            find = true;
            break;
        }
        p = p->next;
    }
    ASSERT(find, ("'olds' is not inside in pheader"));
    #endif

    news->prev = olds->prev;
    news->next = olds->next;
    if (olds->prev != NULL) {
        olds->prev->next = news;
    }
    if (olds->next != NULL) {
        olds->next->prev = news;
    }
    if (olds == *pheader) {
        *pheader = news;
    }
    olds->next = olds->prev = NULL;
}


template <class T>
inline T * removehead(T ** pheader)
{
    if (pheader == NULL || *pheader == NULL) return NULL;
    T * t = *pheader;
    *pheader = t->next;
    if (*pheader != NULL) {
        (*pheader)->prev = NULL;
    }
    t->next = t->prev = NULL;
    return t;
}


template <class T>
inline T * removehead_single_list(T ** pheader)
{
    if (pheader == NULL || *pheader == NULL) return NULL;
    T * t = *pheader;
    *pheader = t->next;
    t->next = NULL;
    return t;
}


template <class T>
inline T * removetail(T ** pheader)
{
    if (pheader == NULL || *pheader == NULL) { return NULL; }
    T * t = *pheader;
    while (t->next != NULL) { t = t->next; }
    remove(pheader, t);
    return t;
}


//Insert one elem 't' before 'marker'.
template <class T>
inline void insertbefore_one(T ** head, T * marker, T * t)
{
    if (t == NULL) return;
    ASSERT(head, ("absent parameter"));
    if (t == marker) return;
    if (*head == NULL) {
        ASSERT(marker == NULL, ("marker must be NULL"));
        *head = t;
        return;
    }

    if (marker == *head) {
        //'marker' is head, and replace head.
        t->prev = NULL;
        t->next = marker;
        marker->prev = t;
        *head = t;
        return;
    }

    ASSERT(marker->prev != NULL, ("marker is head"));
    marker->prev->next = t;
    t->prev = marker->prev;
    t->next = marker;
    marker->prev = t;
}


//Insert a list that leading by 't' before 'marker'.
//'head': function might modify the header of list.
//'t': the head element of list, that to be inserted.
template <class T>
inline void insertbefore(T ** head, T * marker, T * t)
{
    if (t == NULL) { return; }
    ASSERT(head, ("absent parameter"));
    if (t == marker) { return; }
    if (*head == NULL) {
        ASSERT(marker == NULL, ("marker must be NULL"));
        *head = t;
        return;
    }

    if (marker == *head) {
        //'marker' is head, and replace head.
        ASSERT(t->prev == NULL, ("t is not the first element"));
        add_next(&t, *head);
        *head = t;
        return;
    }

    ASSERT(marker->prev != NULL, ("marker should not be head"));
    if (marker->prev != NULL) {
        marker->prev->next = t;
        t->prev = marker->prev;
    }

    t = get_last(t);
    t->next = marker;
    marker->prev = t;
}


//Insert t into list immediately that following 'marker'.
//e.g: a->maker->b->c
//    output is: a->maker->t->b->c
//Return header in 'marker' if list is empty.
template <class T>
inline void insertafter_one(T ** marker, T * t)
{
    if (marker == NULL || t == NULL) return;
    if (t == *marker) return;
    if (*marker == NULL) {
        *marker = t;
        return;
    }
    T * last = get_last(t);
    if ((*marker)->next != NULL) {
        (*marker)->next->prev = last;
        last->next = (*marker)->next;
    }
    (*marker)->next = t;
    t->prev = *marker;
}


//Append t into head of list.
//e.g: given head->a->b->c, and t1->t2.
//    output is: t1->t2->a->b->c
//This function will update the head of list.
template <class T>
inline void append_head(T ** head, T * t)
{
    if (*head == NULL) {
        *head = t;
        return;
    }

    T * last = get_last(t);
    (*head)->prev = last;
    last->next = *head;
    *head = t;
}


//Insert t into marker's list as the subsequent element.
//e.g: a->maker->b->c,  and t->x->y
//output is: a->maker->t->x->y->b->c.
template <class T>
inline void insertafter(T ** marker, T * t)
{
    if (marker == NULL || t == NULL) return;
    if (t == *marker) return;
    if (*marker == NULL) {
        *marker = t;
        return;
    }

    if ((*marker)->next != NULL) {
        T * last = get_last(t);
        (*marker)->next->prev = last;
        last->next = (*marker)->next;
    }
    t->prev = *marker;
    (*marker)->next = t;
}


//Reverse list, return the new list-head.
template <class T>
inline T * reverse_list(T * t)
{
    T * head = t;
    while (t != NULL) {
        T * tmp = t->prev;
        t->prev = t->next;
        t->next = tmp;
        head = t;
        t = t->prev;
    }
    return head;
}


//Double Chain Container.
#define C_val(c)    ((c)->value)
#define C_next(c)   ((c)->next)
#define C_prev(c)   ((c)->prev)
template <class T> class C {
public:
    C<T> * prev;
    C<T> * next;
    T value;

public:
    C() { init(); }
    COPY_CONSTRUCTOR(C<T>);

    void init()
    {
        prev = next = NULL;
        value = T(0); //The default value of container.
    }

    T val() { return value; }
};


//Single Chain Container.
#define SC_val(c)    ((c)->value)
#define SC_next(c)   ((c)->next)
template <class T> class SC {
public:
    SC<T> * next;
    T value;

public:
    SC() { init(); }
    COPY_CONSTRUCTOR(SC<T>);

    void init()
    {
        next = NULL;
        value = T(0);
    }

    T val() { return value; }
};



//FREE-List
//
//T refer to basis element type.
//    e.g: Suppose variable type is 'VAR*', then T is 'VAR'.
//
//For easing implementation, there are 2 fields should be declared in T,
//    struct T {
//        T * next;
//        T * prev;
//        ... //other field
//    }
template <class T>
class FreeList {
public:
    BOOL m_is_clean;
    T * m_tail;

public:
    FreeList()
    {
        m_is_clean = true;
        m_tail = NULL;
    }
    COPY_CONSTRUCTOR(FreeList);
    ~FreeList()
    { m_tail = NULL; }

    UINT count_mem() const { return sizeof(FreeList<T>); }

    //Note the element in list should be freed by user.
    void clean()
    { m_tail = NULL; }

    //True if invoke memset when user query free element.
    void set_clean(bool is_clean)
    { m_is_clean = (BYTE)is_clean; }

    //Add t to tail of the list.
    //Do not clean t's content.
    inline void add_free_elem(T * t)
    {
        if (t == NULL) {return;}
        ASSERT0(t->next == NULL && t->prev == NULL); //clean by user.
        if (m_tail == NULL) {
            m_tail = t;
            return;
        }
        t->prev = m_tail;
        m_tail->next = t;
        m_tail = t;
    }

    //Remove an element from tail of list.
    inline T * get_free_elem()
    {
        if (m_tail == NULL) { return NULL; }
        T * t = m_tail;
        m_tail = m_tail->prev;
        if (m_tail != NULL) {
            ASSERT0(t->next == NULL);
            m_tail->next = NULL;
            m_is_clean ? memset(t, 0, sizeof(T)) : t->prev = NULL;
        } else {
            ASSERT0(t->prev == NULL && t->next == NULL);
            if (m_is_clean) {
                memset(t, 0, sizeof(T));
            }
        }
        return t;
    }
};
//END FreeList



//Dual Linked List.
//NOTICE:
//    The following operations are the key points which you should
//    pay attention to:
//    1.    If you REMOVE one element, its container will be collect by FREE-List.
//        So if you need a new container, please check the FREE-List first,
//        accordingly, you should first invoke 'get_free_list' which get free
//        containers out from 'm_free_list'.
//      2.    If you want to invoke APPEND, please call 'newc' first to
//        allocate a new container memory space, record your elements in
//        container, then APPEND it at list.
template <class T, class Allocator = allocator<T> > class List {
protected:
    UINT m_elem_count;
    C<T> * m_head;
    C<T> * m_tail;
    Allocator pool;

    //It is a marker that used internally by List. Some function will
    //update the variable, see comments.
    C<T> * m_cur;

    //Hold the freed containers for next request.
    FreeList<C<T> > m_free_list;
public:
    List() { init(); }
    COPY_CONSTRUCTOR(List);
    ~List() { destroy(); }

    void init()
    {
        m_elem_count = 0;
        m_head = m_tail = m_cur = NULL;
        m_free_list.clean();
        m_free_list.set_clean(false);
    }

    void destroy()
    {
        C<T> * ct = m_free_list.m_tail;
        while (ct != NULL) {
            C<T> * t = ct;
            ct = ct->prev;

            //Do not need to invoke destructor of C<T>.
            operator delete(t, &pool);
        }
        ct = m_head;
        while (ct != NULL) {
            C<T> * t = ct;
            ct = ct->next;

            //Do not need to invoke destructor of C<T>.
            operator delete(t, &pool);
        }
        m_free_list.clean();
        m_elem_count = 0;
        m_head = m_tail = m_cur = NULL;
    }

    //Return the end of the list.
    C<T> const* end() const { return NULL; }

    inline C<T> * newc()
    {
        //allocator<T> p;
        C<T> * c = m_free_list.get_free_elem();
        if (c == NULL) {
            return new (&pool) C<T>();
        } else {
            C_val(c) = T(0);
        }
        return c;
    }

    //Clean list, and add element containers to free list.
    void clean()
    {
        C<T> * c = m_head;
        while (c != NULL) {
            C<T> * next = C_next(c);
            C_prev(c) = C_next(c) = NULL;
            m_free_list.add_free_elem(c);
            c = next;
        }
        m_elem_count = 0;
        m_head = m_tail = m_cur = NULL;
    }

    void copy(IN List<T> & src)
    {
        clean();
        T t = src.get_head();
        for (INT n = src.get_elem_count(); n > 0; n--) {
            append_tail(t);
            t = src.get_next();
        }
    }

    UINT count_mem() const
    {
        UINT count = 0;
        count += sizeof(m_elem_count);
        count += sizeof(m_head);
        count += sizeof(m_tail);
        count += sizeof(m_cur);
        count += m_free_list.count_mem();

        C<T> * ct = m_free_list.m_tail;
        while (ct != NULL) {
            count += sizeof(C<T>);
            ct = ct->next;
        }

        ct = m_head;
        while (ct != NULL) {
            count += sizeof(C<T>);
            ct = ct->next;
        }
        return count;
    }

    C<T> * append_tail(T t)
    {
        C<T> * c = newc();
        ASSERT(c != NULL, ("newc return NULL"));
        C_val(c) = t;
        append_tail(c);
        return c;
    }

    void append_tail(C<T> * c)
    {
        if (m_head == NULL) {
            ASSERT(m_tail == NULL, ("tail should be NULL"));
            m_head = m_tail = c;
            C_next(m_head) = C_prev(m_head) = NULL;
            m_elem_count = 1;
            return;
        }
        C_prev(c) = m_tail;
        C_next(m_tail) = c;
        m_tail = c;
        ASSERT0(C_next(c) == NULL);
        m_elem_count++;
        return;
    }


    //This function will remove all elements in 'src' and
    //append to tail of current list.
    //Note 'src' will be clean.
    void append_tail(IN OUT List<T> & src)
    {
        if (src.m_head == NULL) { return; }
        if (m_tail == NULL) {
            m_head = src.m_head;
            m_tail = src.m_tail;
            m_elem_count = src.m_elem_count;
            src.m_head = NULL;
            src.m_tail = NULL;
            src.m_elem_count = 0;
            return;
        }

        C_next(m_tail) = src.m_head;
        C_prev(src.m_head) = m_tail;
        m_elem_count += src.m_elem_count;

        ASSERT0(src.m_tail != NULL);
        m_tail = src.m_tail;

        src.m_head = NULL;
        src.m_tail = NULL;
        src.m_elem_count = 0;
    }

    //This function copy elements in 'src' and
    //append to tail of current list.
    //'src' is unchanged.
    void append_and_copy_to_tail(List<T> const& src)
    {
        C<T> * t = src.m_head;
        if (t == NULL) { return; }

        if (m_head == NULL) {
            C<T> * c  = newc();
            ASSERT0(c);
            C_val(c) = C_val(t);

            ASSERT(m_tail == NULL, ("tail should be NULL"));
            m_head = m_tail = c;
            ASSERT0(C_next(c) == NULL && C_prev(c) == NULL);
            t = C_next(t);
        }

        for (; t != T(0); t = C_next(t)) {
            C<T> * c  = newc();
            ASSERT0(c);
            C_val(c) = C_val(t);
            C_prev(c) = m_tail;
            C_next(m_tail) = c;
            m_tail = c;
        }
        C_next(m_tail) = NULL;
        m_elem_count += src.get_elem_count();
    }

    //Append value t to head of list.
    C<T> * append_head(T t)
    {
        C<T> * c  = newc();
        ASSERT(c, ("newc return NULL"));
        C_val(c) = t;
        append_head(c);
        return c;
    }

    //Append container to head of list.
    void append_head(C<T> * c)
    {
        if (m_head == NULL) {
            ASSERT(m_tail == NULL, ("tail should be NULL"));
            m_head = m_tail = c;
            C_next(m_head) = C_prev(m_head) = NULL;
            m_elem_count = 1;
            return;
        }
        C_next(c) = m_head;
        C_prev(m_head) = c;
        m_head = c;

        ASSERT0(C_prev(c) == NULL);
        //C_prev(m_head) = NULL;

        m_elem_count++;
        return;
    }

    //This function will remove all elements in 'src' and
    //append to current list head.
    //Note 'src' will be clean.
    void append_head(IN OUT List<T> & src)
    {
        if (src.m_head == NULL) { return; }
        if (m_tail == NULL) {
            m_head = src.m_head;
            m_tail = src.m_tail;
            m_elem_count = src.m_elem_count;
            src.m_head = NULL;
            src.m_tail = NULL;
            src.m_elem_count = 0;
            return;
        }

        ASSERT0(src.m_tail);
        C_prev(m_head) = src.m_tail;
        C_next(src.m_tail) = m_head;
        m_elem_count += src.m_elem_count;
        m_head = src.m_head;
        src.m_head = NULL;
        src.m_tail = NULL;
        src.m_elem_count = 0;
    }

    //This function copy all elements in 'src' and
    //append to current list head.
    void append_and_copy_to_head(List<T> const& src)
    {
        C<T> * t = src.m_tail;
        if (t == NULL) { return; }

        if (m_head == NULL) {
            C<T> * c  = newc();
            ASSERT0(c);
            C_val(c) = C_val(t);

            ASSERT(m_tail == NULL, ("tail should be NULL"));
            m_head = m_tail = c;
            ASSERT0(C_next(c) == NULL && C_prev(c) == NULL);
            t = C_prev(t);
        }

        for (; t != T(0); t = C_prev(t)) {
            C<T> * c  = newc();
            ASSERT0(c);
            C_val(c) = C_val(t);
            C_next(c) = m_head;
            C_prev(m_head) = c;
            m_head = c;
        }
        C_prev(m_head) = NULL;
        m_elem_count += src.get_elem_count();
    }

    //Return true if p is in current list.
    bool in_list(C<T> const* p) const
    {
        if (p == NULL) { return true; }
        C<T> const* t = m_head;
        while (t != NULL) {
            if (t == p) { return true; }
            t = C_next(t);
        }
        return false;
    }

    //Insert value t before marker.
    //Note this function will do searching for t and marker, so it is
    //a costly function, and used it be carefully.
    C<T> * insert_before(T t, T marker)
    {
        ASSERT(t != marker, ("element of list cannot be identical"));
        if (m_head == NULL || marker == C_val(m_head)) {
            return append_head(t);
        }

        C<T> * c = newc();
        ASSERT0(c);
        C_val(c) = t;

        ASSERT0(m_tail);
        if (marker == m_tail->val()) {
            if (C_prev(m_tail) != NULL) {
                C_next(C_prev(m_tail)) = c;
                C_prev(c) = C_prev(m_tail);
            }
            C_next(c) = m_tail;
            C_prev(m_tail) = c;
        } else {
            //find marker
            C<T> * mc = m_head;
            while (mc != NULL) {
                if (mc->val() == marker) {
                    break;
                }
                mc = C_next(mc);
            }
            if (mc == NULL) { return NULL; }

            if (C_prev(mc) != NULL) {
                C_next(C_prev(mc)) = c;
                C_prev(c) = C_prev(mc);
            }
            C_next(c) = mc;
            C_prev(mc) = c;
        }
        m_elem_count++;
        return c;
    }

    //Insert container 'c' into list before the 'marker'.
    void insert_before(IN C<T> * c, IN C<T> * marker)
    {
        ASSERT0(marker && c && C_prev(c) == NULL && C_next(c) == NULL);
        ASSERT0(c != marker);
        ASSERT0(m_tail);

        #ifdef _SLOW_CHECK_
        ASSERT0(in_list(marker));
        #endif

        if (C_prev(marker) != NULL) {
            C_next(C_prev(marker)) = c;
            C_prev(c) = C_prev(marker);
        }

        C_next(c) = marker;
        C_prev(marker) = c;
        m_elem_count++;
        if (marker == m_head) {
            m_head = c;
        }
    }

    //Insert 't' into list before the 'marker'.
    C<T> * insert_before(T t, IN C<T> * marker)
    {
        C<T> * c = newc();
        ASSERT(c, ("newc return NULL"));
        C_val(c) = t;
        insert_before(c, marker);
        return c;
    }

    //Insert 'src' before 'marker', and return the CONTAINER
    //of src head and src tail.
    //This function move all element in 'src' into current list.
    void insert_before(
            IN OUT List<T> & src,
            IN C<T> * marker,
            OUT C<T> ** list_head_ct = NULL,
            OUT C<T> ** list_tail_ct = NULL)
    {
        if (src.m_head == NULL) { return; }
        ASSERT0(m_head && marker);
        #ifdef _SLOW_CHECK_
        ASSERT0(in_list(marker));
        #endif
        ASSERT0(src.m_tail);

        if (C_prev(marker) != NULL) {
            C_next(C_prev(marker)) = src.m_head;
            C_prev(src.m_head) = C_prev(marker);
        }

        C_next(src.m_tail) = marker;
        C_prev(marker) = src.m_tail;
        m_elem_count += src.m_elem_count;

        if (marker == m_head) {
            m_head = src.m_head;
        }

        if (list_head_ct != NULL) {
            *list_head_ct = src.m_head;
        }

        if (list_tail_ct != NULL) {
            *list_tail_ct = src.m_tail;
        }

        src.m_head = NULL;
        src.m_tail = NULL;
        src.m_elem_count = 0;
    }

    //Insert 'list' before 'marker', and return the CONTAINER
    //of list head and list tail.
    void insert_and_copy_before(
            List<T> const& list,
            T marker,
            OUT C<T> ** list_head_ct = NULL,
            OUT C<T> ** list_tail_ct = NULL)
    {
        C<T> * ct = NULL;
        find(marker, &ct);
        insert_before(list, ct, list_head_ct, list_tail_ct);
    }

    //Insert 'list' before 'marker', and return the CONTAINER
    //of list head and list tail.
    void insert_and_copy_before(
            List<T> const& list,
            IN C<T> * marker,
            OUT C<T> ** list_head_ct = NULL,
            OUT C<T> ** list_tail_ct = NULL)
    {
        if (list.m_head == NULL) { return; }
        ASSERT0(marker);
        C<T> * list_ct = list.m_tail;
        marker = insert_before(list_ct->val(), marker);
        if (list_tail_ct != NULL) {
            *list_tail_ct = marker;
        }
        list_ct = C_prev(list_ct);
        C<T> * prev_ct = marker;

        for (; list_ct != NULL; list_ct = C_prev(list_ct)) {
            marker = insert_before(list_ct->val(), marker);
            prev_ct = marker;
        }

        if (list_head_ct != NULL) {
            *list_head_ct = prev_ct;
        }
    }

    C<T> * insert_after(T t, T marker)
    {
        ASSERT(t != marker,("element of list cannot be identical"));
        if (m_tail == NULL || marker == m_tail->val()) {
            append_tail(t);
            return m_tail;
        }
        ASSERT(m_head != NULL, ("Tail is non empty, but head is NULL!"));
        C<T> * c = newc();
        ASSERT(c != NULL, ("newc return NULL"));
        C_val(c) = t;
        if (marker == m_head->val()) {
            if (C_next(m_head) != NULL) {
                C_prev(C_next(m_head)) = c;
                C_next(c) = C_next(m_head);
            }
            C_prev(c) = m_head;
            C_next(m_head) = c;
        } else {
            //find marker
            C<T> * mc = m_head;
            while (mc != NULL) {
                if (mc->val() == marker) {
                    break;
                }
                mc = C_next(mc);
            }
            if (mc == NULL) return NULL;
            if (C_next(mc) != NULL) {
                C_prev(C_next(mc)) = c;
                C_next(c) = C_next(mc);
            }
            C_prev(c) = mc;
            C_next(mc) = c;
        }
        m_elem_count++;
        return c;
    }

    //Insert 'c' into list after the 'marker'.
    void insert_after(IN C<T> * c, IN C<T> * marker)
    {
        ASSERT0(marker && c && C_prev(c) == NULL && C_next(c) == NULL);
        if (c == marker) { return; }

        ASSERT0(m_head);
        #ifdef _SLOW_CHECK_
        ASSERT0(in_list(marker));
        #endif

        if (C_next(marker) != NULL) {
            C_prev(C_next(marker)) = c;
            C_next(c) = C_next(marker);
        }
        C_prev(c) = marker;
        C_next(marker) = c;
        if (marker == m_tail) {
            m_tail = c;
        }
        m_elem_count++;
    }

    //Insert 't' into list after the 'marker'.
    C<T> * insert_after(T t, IN C<T> * marker)
    {
        C<T> * c = newc();
        ASSERT(c != NULL, ("newc return NULL"));
        C_val(c) = t;
        insert_after(c, marker);
        return c;
    }

    //Insert 'src' after 'marker', and return the CONTAINER
    //of src head and src tail.
    //This function move all element in 'src' into current list.
    void insert_after(
            IN OUT List<T> & src,
            IN C<T> * marker,
            OUT C<T> ** list_head_ct = NULL,
            OUT C<T> ** list_tail_ct = NULL)
    {
        if (src.m_head == NULL) { return; }
        ASSERT0(m_head && marker);
        #ifdef _SLOW_CHECK_
        ASSERT0(in_list(marker));
        #endif
        ASSERT0(src.m_tail);

        if (C_next(marker) != NULL) {
            C_prev(C_next(marker)) = src.m_tail;
            C_next(src.m_tail) = C_next(marker);
        }

        C_prev(src.m_head) = marker;
        C_next(marker) = src.m_head;
        m_elem_count += src.m_elem_count;

        if (marker == m_tail) {
            m_tail = src.m_tail;
        }

        if (list_head_ct != NULL) {
            *list_head_ct = src.m_head;
        }

        if (list_tail_ct != NULL) {
            *list_tail_ct = src.m_tail;
        }

        src.m_head = NULL;
        src.m_tail = NULL;
        src.m_elem_count = 0;
    }

    //Insert 'list' after 'marker', and return the CONTAINER
    //of list head and list tail.
    void insert_and_copy_after(
            List<T> const& list,
            T marker,
            OUT C<T> ** list_head_ct = NULL,
            OUT C<T> ** list_tail_ct = NULL)
    {
        C<T> * ct = NULL;
        find(marker, &ct);
        insert_after(list, ct, list_head_ct, list_tail_ct);
    }

    //Insert 'list' after 'marker', and return the CONTAINER
    //of head and tail of members in 'list'.
    void insert_and_copy_after(
            List<T> const& list,
            IN C<T> * marker,
            OUT C<T> ** list_head_ct = NULL,
            OUT C<T> ** list_tail_ct = NULL)
    {
        if (list.m_head == NULL) { return; }

        ASSERT0(marker);
        C<T> * list_ct = list.m_head;
        marker = insert_after(list_ct->val(), marker);

        if (list_head_ct != NULL) {
            *list_head_ct = marker;
        }

        list_ct = C_next(list_ct);
        C<T> * prev_ct = marker;

        for (; list_ct != NULL; list_ct = C_next(list_ct)) {
            marker = insert_after(list_ct->val(), marker);
            prev_ct = marker;
        }

        if (list_tail_ct != NULL) {
            *list_tail_ct = prev_ct;
        }
    }

    UINT get_elem_count() const { return m_elem_count; }

    T get_cur() const
    {
        if (m_cur == NULL) { return T(0); }
        return m_cur->val();
    }

    //Return m_cur and related container, and it does not modify m_cur.
    T get_cur(IN OUT C<T> ** holder) const
    {
        if (m_cur == NULL) {
            *holder = NULL;
            return T(0);
        }
        ASSERT0(holder != NULL);
        *holder = m_cur;
        return m_cur->val();
    }

    //Get tail of list, return the CONTAINER.
    //This function does not modify m_cur.
    T get_tail(OUT C<T> ** holder) const
    {
        ASSERT0(holder);
        *holder = m_tail;
        if (m_tail != NULL) {
            return m_tail->val();
        }
        return T(0);
    }

    //Get tail of list, return the CONTAINER.
    //This function will modify m_cur.
    T get_tail()
    {
        m_cur = m_tail;
        if (m_tail != NULL) {
            return m_tail->val();
        }
        return T(0);
    }

    //Get head of list, return the CONTAINER.
    //This function will modify m_cur.
    T get_head()
    {
        m_cur = m_head;
        if (m_head != NULL) {
            return m_head->val();
        }
        return T(0);
    }

    //Get head of list, return the CONTAINER.
    //This function does not modify m_cur.
    T get_head(OUT C<T> ** holder) const
    {
        ASSERT0(holder);
        *holder = m_head;
        if (m_head != NULL) {
            return m_head->val();
        }
        return T(0);
    }

    //Get element next to m_cur.
    //This function will modify m_cur.
    T get_next()
    {
        if (m_cur == NULL || m_cur->next == NULL) {
            m_cur = NULL;
            return T(0);
        }
        m_cur = m_cur->next;
        return m_cur->val();
    }

    //Return next container.
    //Caller could get the element via C_val or val().
    //This function does not modify m_cur.
    C<T> * get_next(IN C<T> * holder) const
    {
        ASSERT0(holder);
        return C_next(holder);
    }

    //Return list member and update holder to next member.
    //This function does not modify m_cur.
    T get_next(IN OUT C<T> ** holder) const
    {
        ASSERT0(holder && *holder);
        *holder = C_next(*holder);
        if (*holder != NULL) {
            return C_val(*holder);
        }
        return T(0);
    }

    //Get element previous to m_cur.
    //This function will modify m_cur.
    T get_prev()
    {
        if (m_cur == NULL || m_cur->prev == NULL) {
            m_cur = NULL;
            return T(0);
        }
        m_cur = m_cur->prev;
        return m_cur->val();
    }

    //Return list member and update holder to prev member.
    //This function does not modify m_cur.
    T get_prev(IN OUT C<T> ** holder) const
    {
        ASSERT0(holder && *holder);
        *holder = C_prev(*holder);
        if (*holder != NULL) {
            return C_val(*holder);
        }
        return T(0);
    }

    //Return prev container.
    //Caller could get the element via C_val or val().
    //This function does not modify m_cur.
    C<T> * get_prev(IN C<T> * holder) const
    {
        ASSERT0(holder);
        return C_prev(holder);
    }

    //Get element for nth at tail.
    //'n': starting at 0.
    //This function will modify m_cur.
    T get_tail_nth(UINT n, IN OUT C<T> ** holder = NULL)
    {
        ASSERT(n < m_elem_count,("Access beyond list"));
        m_cur = NULL;
        if (m_elem_count == 0) { return T(0); }
        C<T> * c;
        if (n <= (m_elem_count >> 1)) { // n<floor(elem_count,2)
            c = m_tail;
            while (n > 0) {
                c = C_prev(c);
                n--;
            }
        } else {
            return get_head_nth(m_elem_count - n - 1);
        }

        m_cur = c;

        if (holder != NULL) {
            *holder = c;
        }
        return c->val();
    }

    //Get element for nth at head.
    //'n': getting start with zero.
    //This function will modify m_cur.
    T get_head_nth(UINT n, IN OUT C<T> ** holder = NULL)
    {
        ASSERT(n < m_elem_count,("Access beyond list"));
        m_cur = NULL;
        if (m_head == NULL) {
            return T(0);
        }
        C<T> * c;
        if (n <= (m_elem_count >> 1)) { // n<floor(elem_count,2)
            c = m_head;
            while (n > 0) {
                c = C_next(c);
                n--;
            }
        } else {
            return get_tail_nth(m_elem_count - n - 1);
        }
        m_cur = c;
        if (holder != NULL) {
            *holder = c;
        }
        return c->val();
    }

    bool find(IN T t, OUT C<T> ** holder = NULL) const
    {
        C<T> * c = m_head;
        while (c != NULL) {
            if (c->val() == t) {
                if (holder != NULL) {
                    *holder = c;
                }
                return true;
            }
            c = C_next(c);
        }

        if (holder != NULL) {
            *holder = NULL;
        }
        return false;
    }

    //Reverse list.
    void reverse()
    {
        C<T> * next_ct;
        for (C<T> * ct = m_head; ct != NULL; ct = next_ct) {
            next_ct = ct->next;
            ct->next = ct->prev;
            ct->prev = next_ct;
        }
        next_ct = m_head;
        m_head = m_tail;
        m_tail = next_ct;
    }

    //Remove from list directly.
    T remove(C<T> * holder)
    {
        ASSERT0(holder);
        if (holder == m_cur) {
            m_cur = m_cur->next;
        }

        ASSERT(m_head, ("list is empty"));

        if (m_head == holder) {
            return remove_head();
        }

        if (m_tail == holder) {
            return remove_tail();
        }

        ASSERT(C_prev(holder) && C_next(holder), ("illegal t in list"));

        C_next(C_prev(holder)) = C_next(holder);
        C_prev(C_next(holder)) = C_prev(holder);
        m_elem_count--;
        C_prev(holder) = C_next(holder) = NULL;
        m_free_list.add_free_elem(holder);
        return holder->val();
    }

    //Remove from list, and searching for 't' begin at head
    T remove(T t)
    {
        if (m_head == NULL) { return T(0); }
        if (m_head->val() == t) { return remove_head(); }
        if (m_tail->val() == t) { return remove_tail(); }

        C<T> * c = m_head;
        while (c != NULL) {
            if (c->val() == t) { break; }
            c = C_next(c);
        }

        if (c == NULL) { return T(0); }

        return remove(c);
    }

    //Remove from tail.
    T remove_tail()
    {
        if (m_tail == NULL) { return T(0); }

        C<T> * c = NULL;
        if (C_prev(m_tail) == NULL) {
            //tail is the only one
            ASSERT(m_tail == m_head && m_elem_count == 1,
                   ("illegal list-remove"));
            c = m_tail;
            m_head = m_tail = m_cur = NULL;
        } else {
            c = m_tail;
            if (m_cur == m_tail) {
                m_cur = C_prev(m_tail);
            }
            m_tail = C_prev(m_tail);
            C_next(m_tail) = NULL;
            C_prev(c) = NULL;
        }

        m_elem_count--;
        m_free_list.add_free_elem(c);
        return c->val();
    }

    //Remove from head.
    T remove_head()
    {
        C<T> * c = NULL;
        if (m_head == NULL) { return T(0); }
        if (C_next(m_head) == NULL) {
            //list_head is the only one
            ASSERT(m_tail == m_head && m_elem_count == 1,
                   ("illegal list-remove"));
            c = m_head;
            m_head = m_tail = m_cur = NULL;
        } else {
            c = m_head;
            if (m_cur == m_head) {
                m_cur = C_next(m_head);
            }
            m_head = C_next(m_head);
            C_prev(m_head) = NULL;
            C_prev(c) = C_next(c) = NULL;
        }

        m_free_list.add_free_elem(c);
        m_elem_count--;
        return c->val();
    }
};



//Single Linked List Core.
//Encapsulate most operations for single list.
//Note:
//  1. You must invoke init() if the SListCore allocated in mempool.
//  2. The single linked list is different with dual linked list.
//     the dual linked list does not use mempool to hold the container.
//     Compared to dual linked list, single linked list allocate containers
//     in a const size pool.
//  3. Before going to the destructor, even if the containers have
//     been allocated in memory pool, you should invoke clean() to free
//     all of them back to a free list to reuse them.
template <class T> class SListCore {
protected:
    UINT m_elem_count; //list elements counter.
    SC<T> m_head; //list head.

protected:
    SC<T> * new_sc_container(SMemPool * pool)
    {
        ASSERT(pool, ("need mem pool"));
        ASSERT(MEMPOOL_type(pool) == MEM_CONST_SIZE, ("need const size pool"));
        SC<T> * p = (SC<T>*)smpoolMallocConstSize(sizeof(SC<T>), pool);
        ASSERT0(p != NULL);
        ::memset(p, 0, sizeof(SC<T>));
        return p;
    }

    //Check p is the element in list.
    bool in_list(SC<T> const* p) const
    {
        ASSERT0(p);
        if (p == &m_head) { return true; }

        SC<T> const* t = m_head.next;
        while (t != &m_head) {
            if (t == p) { return true; }
            t = t->next;
        }
        return false;
    }

    SC<T> * get_freed_sc(SC<T> ** free_list)
    {
        if (free_list == NULL || *free_list == NULL) { return NULL; }
        SC<T> * t = *free_list;
        *free_list = (*free_list)->next;
        t->next = NULL;
        return t;
    }

    //Find the last element, and return the CONTAINER.
    //This is a costly operation. Use it carefully.
    inline SC<T> * get_tail() const
    {
        SC<T> * c = m_head.next;
        SC<T> * tail = NULL;
        while (c != &m_head) {
            tail = c;
            c = c->next;
        }
        return tail;
    }

    void free_sc(SC<T> * sc, SC<T> ** free_list)
    {
        ASSERT0(free_list);
        sc->next = *free_list;
        *free_list = sc;
    }

    SC<T> * newsc(SC<T> ** free_list, SMemPool * pool)
    {
        SC<T> * c = get_freed_sc(free_list);
        if (c == NULL) {
            c = new_sc_container(pool);
        }
        return c;
    }
public:
    SListCore() { init(); }
    COPY_CONSTRUCTOR(SListCore);
    ~SListCore()
    {
        //Note: Before invoked the destructor, even if the containers have
        //been allocated in memory pool, you should invoke clean() to
        //free all of them back to a free list to reuse them.
    }

    void init()
    {
        m_elem_count = 0;
        m_head.next = &m_head;
    }

    SC<T> * append_head(T t, SC<T> ** free_list, SMemPool * pool)
    {
        SC<T> * c = newsc(free_list, pool);
        ASSERT(c != NULL, ("newsc return NULL"));
        SC_val(c) = t;
        append_head(c);
        return c;
    }

    void append_head(IN SC<T> * c) { insert_after(c, &m_head); }

    //Find the last element, and add 'c' after it.
    //This is a costly operation. Use it carefully.
    void append_tail(IN SC<T> * c)
    {
        SC<T> * cur = m_head.next;
        SC<T> * prev = &m_head;
        while (cur != &m_head) {
            cur = cur->next;
            prev = cur;
        }
        insert_after(c, prev);
    }

    void copy(IN SListCore<T> & src, SC<T> ** free_list, SMemPool * pool)
    {
        clean(free_list);
        SC<T> * tgt_st = get_head();
        for (SC<T> * src_st = src.get_head();
             src_st != src.end();
             src_st = src.get_next(src_st), tgt_st = get_next(tgt_st)) {
            T t = src_st->val();
            insert_after(t, tgt_st, free_list, pool);
        }
    }

    void clean(SC<T> ** free_list)
    {
        ASSERT0(free_list);
        SC<T> * tail = get_tail();
        if (tail != NULL) {
            tail->next = *free_list;
            *free_list = m_head.next;
            m_head.next = &m_head;
            m_elem_count = 0;
        }

        ASSERT0(m_elem_count == 0);
    }

    UINT count_mem() const
    {
        UINT count = sizeof(m_elem_count);
        count += sizeof(m_head);
        //Do not count SC, they belong to input pool.
        return count;
    }

    //Insert container 'c' after the 'marker'.
    inline void insert_after(IN SC<T> * c, IN SC<T> * marker)
    {
        ASSERT0(marker && c && c != marker);
        #ifdef _SLOW_CHECK_
        ASSERT0(in_list(marker));
        #endif

        c->next = marker->next;
        marker->next = c;
        m_elem_count++;
    }

    //Insert value 't' after the 'marker'.
    //free_list: a list record free containers.
    //pool: memory pool which is used to allocate container.
    inline SC<T> * insert_after(
            T t,
            IN SC<T> * marker,
            SC<T> ** free_list,
            SMemPool * pool)
    {
        ASSERT0(marker);
        #ifdef _SLOW_CHECK_
        ASSERT0(in_list(marker));
        #endif

        SC<T> * c = newsc(free_list, pool);
        ASSERT(c != NULL, ("newc return NULL"));

        SC_val(c) = t;
        insert_after(c, marker);
        return c;
    }

    UINT get_elem_count() const { return m_elem_count; }

    //Return the end of the list.
    SC<T> const * end() const { return &m_head; }

    //Get head of list, return the CONTAINER.
    //You could iterate the list via comparing the container with end().
    SC<T> * get_head() const { return m_head.next; }

    //Return the next container.
    SC<T> * get_next(IN SC<T> * holder) const
    {
        ASSERT0(holder);
        return SC_next(holder);
    }

    //Find 't' in list, return the container in 'holder' if 't' existed.
    //The function is regular list search, and has O(n) complexity.
    //Note that this is costly operation. Use it carefully.
    bool find(IN T t, OUT SC<T> ** holder = NULL) const
    {
        SC<T> * c = m_head.next;
        while (c != &m_head) {
            if (c->val() == t) {
                if (holder != NULL) {
                    *holder = c;
                }
                return true;
            }
            c = c->next;
        }

        if (holder != NULL) {
            *holder = NULL;
        }
        return false;
    }

    //Remove 't' out of list, return true if find t, otherwise return false.
    //Note that this is costly operation. Use it carefully.
    bool remove(T t, SC<T> ** free_list)
    {
        SC<T> * c = m_head.next;
        SC<T> * prev = &m_head;
        while (c != &m_head) {
            if (c->val() == t) { break; }

            prev = c;
            c = c->next;
        }

        if (c == &m_head) { return false; }

        remove(prev, c, free_list);

        return true;
    }

    //Return the element removed.
    //'prev': the previous one element of 'holder'.
    T remove(SC<T> * prev, SC<T> * holder, SC<T> ** free_list)
    {
        ASSERT0(holder);
        ASSERT(m_head.next != &m_head, ("list is empty"));
        ASSERT0(prev);

        ASSERT(prev->next == holder, ("not prev one"));
        prev->next = holder->next;
        m_elem_count--;
        T t = SC_val(holder);
        free_sc(holder, free_list);
        return t;
    }

    //Return the element removed.
    T remove_head(SC<T> ** free_list)
    {
        if (m_head.next == &m_head) { return T(0); }

        SC<T> * c = m_head.next;
        m_head.next = c->next;
        T t = c->val();
        free_sc(c, free_list);
        m_elem_count--;
        return t;
    }
};
//END SListCore


//Single List
//NOTICE:
//    The following operations are the key points which you should attention to:
//
//    1. You must invoke init() if the SList allocated in mempool.
//    2. Before going to the destructor, even if the containers have
//       been allocated in memory pool, you should invoke clean() to free
//       all of them back to a free list to reuse them.
//    3. If you REMOVE one element, its container will be collect by FREE-List.
//       So if you need a new container, please check the FREE-List first,
//       accordingly, you should first invoke 'get_free_list' which get free
//       containers out from 'm_free_list'.
//    4. The single linked list is different with dual linked list.
//       the dual linked list does not use mempool to hold the container.
//       Compared to dual linked list, single linked list allocate containers
//       in a const size pool.
//       Invoke init() to do initialization if you allocate SList by malloc().
//    5. Compare the iterator with end() to determine if meeting the end of list.
//    6. Byte size of element in Const Pool is equal to sizeof(SC<T>).
//
//    Usage:SMemPool * pool = smpoolCreate(sizeof(SC<T>) * n, MEM_CONST_SIZE);
//          SList<T> list(pool);
//          SList<T> * list2 = smpoolMalloc();
//          list2->init(pool);
//          ...
//          smpoolDelete(pool);
template <class T> class SList : public SListCore<T> {
protected:
    SMemPool * m_free_list_pool;
    SC<T> * m_free_list; //Hold for available containers

public:
    SList(SMemPool * pool = NULL)
    {
        //Invoke init() explicitly if SList is allocated from mempool.
        set_pool(pool);
    }
    COPY_CONSTRUCTOR(SList);
    ~SList()
    {
        //destroy() do the same things as the parent class's destructor.
        //So it is not necessary to double call destroy().
        //Note: Before going to the destructor, even if the containers have
        //been allocated in memory pool, you should invoke clean() to
        //free all of them back to a free list to reuse them.
    }

    void init(SMemPool * pool)
    {
        SListCore<T>::init();
        set_pool(pool);
    }

    void set_pool(SMemPool * pool)
    {
        ASSERT(pool == NULL || MEMPOOL_type(pool) == MEM_CONST_SIZE,
               ("need const size pool"));
        m_free_list_pool = pool;
        m_free_list = NULL;
    }

    SMemPool * get_pool() { return m_free_list_pool; }

    SC<T> * append_head(T t)
    {
        ASSERT0(m_free_list_pool);
        return SListCore<T>::append_head(t, &m_free_list, m_free_list_pool);
    }

    void copy(IN SList<T> & src)
    { SListCore<T>::copy(src, &m_free_list, m_free_list_pool); }

    void clean() { SListCore<T>::clean(&m_free_list); }

    UINT count_mem() const
    {
        //Do not count SC containers, they belong to input pool.
        return SListCore<T>::count_mem();
    }

    //Insert 't' into list after the 'marker'.
    SC<T> * insert_after(T t, IN SC<T> * marker)
    {
        ASSERT0(m_free_list_pool);
        return SListCore<T>::insert_after(t,
            marker, &m_free_list, m_free_list_pool);
    }

    //Remove 't' out of list, return true if find t, otherwise return false.
    //Note that this is costly operation.
    bool remove(T t)
    {
        ASSERT0(m_free_list_pool);
        return SListCore<T>::remove(t, &m_free_list);
    }

    //Return element removed.
    //'prev': the previous one element of 'holder'.
    T remove(SC<T> * prev, SC<T> * holder)
    {
        ASSERT0(m_free_list_pool);
        return SListCore<T>::remove(prev, holder, &m_free_list);
    }

    //Return element removed.
    T remove_head()
    {
        ASSERT0(m_free_list_pool);
        return SListCore<T>::remove_head(&m_free_list);
    }
};
//END SList



//The Extended Single List.
//
//This kind of single list has a tail pointer that allows you access
//tail element directly via get_tail(). This will be useful if you
//are going to append element at the tail of list.
//
//Encapsulate most operations for single list.
//
//Note the single linked list is different with dual linked list.
//the dual linked list does not use mempool to hold the container.
//Compared to dual linked list, single linked list allocate containers
//in a const size pool.
template <class T> class SListEx {
protected:
    UINT m_elem_count;
    SC<T> * m_head;
    SC<T> * m_tail;

protected:
    SC<T> * new_sc_container(SMemPool * pool)
    {
        ASSERT(pool, ("need mem pool"));
        ASSERT(MEMPOOL_type(pool) == MEM_CONST_SIZE, ("need const size pool"));
        SC<T> * p = (SC<T>*)smpoolMallocConstSize(sizeof(SC<T>), pool);
        ASSERT0(p);
        ::memset(p, 0, sizeof(SC<T>));
        return p;
    }

    SC<T> * get_freed_sc(SC<T> ** free_list)
    {
        if (free_list == NULL || *free_list == NULL) { return NULL; }
        SC<T> * t = *free_list;
        *free_list = (*free_list)->next;
        t->next = NULL;
        return t;
    }

    void free_sc(SC<T> * sc, SC<T> ** free_list)
    {
        ASSERT0(free_list);
        sc->next = *free_list;
        *free_list = sc;
    }

    SC<T> * newsc(SC<T> ** free_list, SMemPool * pool)
    {
        SC<T> * c = get_freed_sc(free_list);
        if (c == NULL) {
            c = new_sc_container(pool);
        }
        return c;
    }

    //Check p is the element in list.
    bool in_list(SC<T> const* p) const
    {
        if (p == NULL) { return true; }
        SC<T> const* t = m_head;
        while (t != NULL) {
            if (t == p) { return true; }
            t = t->next;
        }
        return false;
    }
public:
    SListEx() { init(); }
    COPY_CONSTRUCTOR(SListEx);
    ~SListEx()
    {
        //Note: Before going to the destructor, even if the containers have
        //been allocated in memory pool, you should free all of them
        //back to a free list to reuse them.
    }

    void init()
    {
        m_elem_count = 0;
        m_head = NULL;
        m_tail = NULL;
    }

    //Return the end of the list.
    SC<T> const* end() const { return NULL; }

    SC<T> * append_tail(T t, SC<T> ** free_list, SMemPool * pool)
    {
        SC<T> * c  = newsc(free_list, pool);
        ASSERT(c != NULL, ("newsc return NULL"));
        SC_val(c) = t;
        append_tail(c);
        return c;
    }

    void append_tail(IN SC<T> * c)
    {
        if (m_head == NULL) {
            ASSERT(m_tail == NULL, ("tail should be NULL"));
            ASSERT0(SC_next(c) == NULL);
            m_head = m_tail = c;
            m_elem_count++;
            return;
        }

        SC_next(m_tail) = c;
        m_tail = c;
        ASSERT0(SC_next(c) == NULL);
        m_elem_count++;
        return;
    }

    SC<T> * append_head(T t, SC<T> ** free_list, SMemPool * pool)
    {
        SC<T> * c = newsc(free_list, pool);
        ASSERT(c != NULL, ("newsc return NULL"));
        SC_val(c) = t;
        append_head(c);
        return c;
    }

    void append_head(IN SC<T> * c)
    {
        #ifdef _DEBUG_
        if (m_head == NULL) {
            ASSERT(m_tail == NULL, ("tail should be NULL"));
            ASSERT0(m_elem_count == 0);
        }
        #endif

        c->next = m_head;
        m_head = c;
        m_elem_count++;
        return;
    }

    void copy(IN SListCore<T> & src, SC<T> ** free_list, SMemPool * pool)
    {
        clean(free_list);
        SC<T> * sct;
        T t = src.get_head(&sct);
        for (INT n = src.get_elem_count(); n > 0; n--) {
            append_tail(t, free_list, pool);
            t = src.get_next(&sct);
        }
    }

    void clean(SC<T> ** free_list)
    {
        ASSERT0(free_list);
        if (m_tail != NULL) {
            m_tail->next = *free_list;
            ASSERT0(m_head);
            *free_list = m_head;
            m_head = m_tail = NULL;
            m_elem_count = 0;
        }
        ASSERT0(m_head == m_tail && m_head == NULL && m_elem_count == 0);
    }

    UINT count_mem() const
    {
        UINT count = sizeof(m_elem_count);
        count += sizeof(m_head);
        count += sizeof(m_tail);
        //Do not count SC, they has been involved in pool.
        return count;
    }

    //Insert 'c' into list after the 'marker'.
    inline void insert_after(IN SC<T> * c, IN SC<T> * marker)
    {
        ASSERT0(marker && c && c != marker);
        #ifdef _SLOW_CHECK_
        ASSERT0(in_list(marker));
        #endif

        c->next = marker->next;
        marker->next = c;
        m_elem_count++;
    }

    //Insert 't' into list after the 'marker'.
    inline SC<T> * insert_after(
            T t,
            IN SC<T> * marker,
            SC<T> ** free_list,
            SMemPool * pool)
    {
        ASSERT0(marker);
        #ifdef _SLOW_CHECK_
        ASSERT0(in_list(marker));
        #endif

        SC<T> * c = newsc(free_list, pool);
        ASSERT(c, ("newc return NULL"));

        SC_val(c) = t;
        insert_after(c, marker);
        return c;
    }

    UINT get_elem_count() const { return m_elem_count; }

    //Get tail of list, return the CONTAINER.
    SC<T> * get_tail() const { return m_tail; }

    //Get head of list, return the CONTAINER.
    SC<T> * get_head() const { return m_head; }

    //Return the next container.
    SC<T> * get_next(IN SC<T> * holder) const
    {
        ASSERT0(holder);
        return SC_next(holder);
    }

    //Find 't' in list, return the container in 'holder' if 't' existed.
    //The function is regular list search, and has O(n) complexity.
    bool find(IN T t, OUT SC<T> ** holder = NULL) const
    {
        SC<T> * c = m_head;
        while (c != NULL) {
            if (c->val() == t) {
                if (holder    != NULL) {
                    *holder = c;
                }
                return true;
            }
            c = c->next;
        }

        if (holder != NULL) {
            *holder = NULL;
        }
        return false;
    }

    //Remove 't' out of list, return true if find t, otherwise return false.
    //Note that this is costly operation.
    bool remove(T t, SC<T> ** free_list)
    {
        if (m_head == NULL) { return false; }
        if (m_head->val() == t) {
            remove_head(free_list);
            return true;
        }

        SC<T> * c = m_head->next;
        SC<T> * prev = m_head;
        while (c != NULL) {
            if (c->val() == t) { break; }
            prev = c;
            c = c->next;
        }

        if (c == NULL) { return false; }

        remove(prev, c, free_list);
        return true;
    }

    //Return the element removed.
    //'prev': the previous one element of 'holder'.
    T remove(SC<T> * prev, SC<T> * holder, SC<T> ** free_list)
    {
        ASSERT0(holder);
        ASSERT(m_head != NULL, ("list is empty"));
        #ifdef _SLOW_CHECK_
        ASSERT0(in_list(prev));
        ASSERT0(in_list(holder));
        #endif

        if (prev == NULL) {
            ASSERT0(holder == m_head);
            m_head = m_head->next;
            if (m_head == NULL) {
                ASSERT0(m_elem_count == 1);
                m_tail = NULL;
            }
        } else {
            ASSERT(prev->next == holder, ("not prev one"));
            prev->next = holder->next;
        }

        if (holder == m_tail) {
            m_tail = prev;
        }

        m_elem_count--;
        T t = SC_val(holder);
        free_sc(holder, free_list);
        return t;
    }

    //Return the element removed.
    T remove_head(SC<T> ** free_list)
    {
        if (m_head == NULL) { return T(0); }

        SC<T> * c = m_head;
        m_head = m_head->next;
        if (m_head == NULL) {
            m_tail = NULL;
        }

        T t = c->val();
        free_sc(c, free_list);
        m_elem_count--;
        return t;
    }
};
//END SListEx



//The Extended List
//
//Add a hash-mapping table upon List in order to speed up the process
//when inserting or removing an element if a 'marker' given.
//
//NOTICE: User must define a mapping class bewteen.
template <class T, class MapTypename2Holder> class EList : public List<T> {
protected:
    MapTypename2Holder m_typename2holder; //map typename 'T' to its list holder.
public:
    EList() {}
    COPY_CONSTRUCTOR(EList);
    virtual ~EList() {} //MapTypename2Holder has virtual function.

    void copy(IN List<T> & src)
    {
        clean();
        T t = src.get_head();
        for (INT n = src.get_elem_count(); n > 0; n--) {
            append_tail(t);
            t = src.get_next();
        }
    }

    void clean()
    {
        List<T>::clean();
        m_typename2holder.clean();
    }

    size_t count_mem() const
    {
        size_t count = m_typename2holder.count_mem();
        count += ((List<T>*)this)->count_mem();
        return count;
    }

    C<T> * append_tail(T t)
    {
        C<T> * c = List<T>::append_tail(t);
        m_typename2holder.setAlways(t, c);
        return c;
    }

    C<T> * append_head(T t)
    {
        C<T> * c = List<T>::append_head(t);
        m_typename2holder.setAlways(t, c);
        return c;
    }

    void append_tail(IN List<T> & list)
    {
        UINT i = 0;
        C<T> * c;
        for (T t = list.get_head(); i < list.get_elem_count();
             i++, t = list.get_next()) {
            c = List<T>::append_tail(t);
            m_typename2holder.setAlways(t, c);
        }
    }

    void append_head(IN List<T> & list)
    {
        UINT i = 0;
        C<T> * c;
        for (T t = list.get_tail(); i < list.get_elem_count();
             i++, t = list.get_prev()) {
            c = List<T>::append_head(t);
            m_typename2holder.setAlways(t, c);
        }
    }

    T remove(T t)
    {
        C<T> * c = m_typename2holder.get(t);
        if (c == NULL) {
            return T(0);
        }
        T tt = List<T>::remove(c);
        m_typename2holder.setAlways(t, NULL);
        return tt;
    }

    T remove(C<T> * holder)
    {
        ASSERT0(m_typename2holder.get(holder->val()) == holder);
        T t = List<T>::remove(holder);
        m_typename2holder.setAlways(t, NULL);
        return t;
    }

    T remove_tail()
    {
        T t = List<T>::remove_tail();
        m_typename2holder.setAlways(t, NULL);
        return t;
    }

    T remove_head()
    {
        T t = List<T>::remove_head();
        m_typename2holder.setAlways(t, NULL);
        return t;
    }

    //NOTICE: 'marker' should have been in the list.
    C<T> * insert_before(T t, T marker)
    {
        C<T> * marker_holder = m_typename2holder.get(marker);
        if (marker_holder == NULL) {
            ASSERT0(List<T>::get_elem_count() == 0);
            C<T> * t_holder = List<T>::append_head(t);
            m_typename2holder.setAlways(t, t_holder);
            return t_holder;
        }
        C<T> * t_holder = List<T>::insert_before(t, marker_holder);
        m_typename2holder.setAlways(t, t_holder);
        return t_holder;
    }

    //NOTICE: 'marker' should have been in the list,
    //and marker will be modified.
    C<T> * insert_before(T t, C<T> * marker)
    {
        ASSERT0(marker && m_typename2holder.get(marker->val()) == marker);
        C<T> * t_holder = List<T>::insert_before(t, marker);
        m_typename2holder.setAlways(t, t_holder);
        return t_holder;
    }

    //NOTICE: 'marker' should have been in the list.
    void insert_before(C<T> * c, C<T> * marker)
    {
        ASSERT0(c && marker && m_typename2holder.get(marker->val()) == marker);
        List<T>::insert_before(c, marker);
        m_typename2holder.setAlways(c->val(), c);
    }

    //NOTICE: 'marker' should have been in the list.
    C<T> * insert_after(T t, T marker)
    {
        C<T> * marker_holder = m_typename2holder.get(marker);
        if (marker_holder == NULL) {
            ASSERT0(List<T>::get_elem_count() == 0);
            C<T> * t_holder = List<T>::append_tail(t);
            m_typename2holder.setAlways(t, t_holder);
            return t_holder;
        }
        C<T> * t_holder = List<T>::insert_after(t, marker_holder);
        m_typename2holder.setAlways(t, t_holder);
        return t_holder;
    }

    //NOTICE: 'marker' should have been in the list.
    C<T> * insert_after(T t, C<T> * marker)
    {
        ASSERT0(marker && m_typename2holder.get(marker->val()) == marker);
        C<T> * marker_holder = marker;
        C<T> * t_holder = List<T>::insert_after(t, marker_holder);
        m_typename2holder.setAlways(t, t_holder);
        return t_holder;
    }

    //NOTICE: 'marker' should have been in the list.
    void insert_after(C<T> * c, C<T> * marker)
    {
        ASSERT0(c && marker && m_typename2holder.get(marker->val()) == marker);
        List<T>::insert_after(c, marker);
        m_typename2holder.setAlways(c->val(), c);
    }

    bool find(T t, C<T> ** holder = NULL) const
    {
        C<T> * c = m_typename2holder.get(t);
        if (c == NULL) {
            return false;
        }
        if (holder != NULL) {
            *holder = c;
        }
        return true;
    }

    MapTypename2Holder * get_holder_map() const { return &m_typename2holder; }

    T get_cur() const //Do NOT update 'm_cur'
    { return List<T>::get_cur(); }

    T get_cur(IN OUT C<T> ** holder) const //Do NOT update 'm_cur'
    { return List<T>::get_cur(holder); }

    T get_next() //Update 'm_cur'
    { return List<T>::get_next(); }

    T get_prev() //Update 'm_cur'
    { return List<T>::get_prev(); }

    T get_next(IN OUT C<T> ** holder) const //Do NOT update 'm_cur'
    { return List<T>::get_next(holder); }

    C<T> * get_next(IN C<T> * holder) const //Do NOT update 'm_cur'
    { return List<T>::get_next(holder); }

    T get_prev(IN OUT C<T> ** holder) const //Do NOT update 'm_cur'
    { return List<T>::get_prev(holder); }

    C<T> * get_prev(IN C<T> * holder) const //Do NOT update 'm_cur'
    { return List<T>::get_prev(holder); }

    T get_next(T marker) const //not update 'm_cur'
    {
        C<T> * holder;
        find(marker, &holder);
        ASSERT0(holder != NULL);
        return List<T>::get_next(&holder);
    }

    T get_prev(T marker) const //not update 'm_cur'
    {
        C<T> * holder;
        find(marker, &holder);
        ASSERT0(holder != NULL);
        return List<T>::get_prev(&holder);
    }

    //Return the container of 'newt'.
    C<T> * replace(T oldt, T newt)
    {
        C<T> * old_holder = m_typename2holder.get(oldt);
        ASSERT(old_holder != NULL, ("old elem not exist"));

        //add new one
        C<T> * new_holder = List<T>::insert_before(newt, old_holder);
        m_typename2holder.setAlways(newt, new_holder);

        //remove old one
        m_typename2holder.setAlways(oldt, NULL);
        List<T>::remove(old_holder);
        return new_holder;
    }
};
//END EList



//STACK
template <class T> class Stack : public List<T> {
public:
    Stack() {}
    COPY_CONSTRUCTOR(Stack);

    void push(T t) { List<T>::append_tail(t); }
    T pop() { return List<T>::remove_tail(); }

    T get_bottom() { return List<T>::get_head(); }
    T get_top() { return List<T>::get_tail(); }
    T get_top_nth(INT n) { return List<T>::get_tail_nth(n); }
    T get_bottom_nth(INT n) { return List<T>::get_head_nth(n); }
};
//END Stack



//Vector
//
//T refer to element type.
//NOTE: zero is reserved and regard it as the default NULL when we
//determine whether an element is exist.
//The vector grow dynamic.
#define SVEC_init(s)        ((s)->m_is_init)
template <class T, UINT GrowSize = 8> class Vector {
protected:
    UINT m_elem_num:31; //The number of element in vector.

    //The number of element when vector is growing.
    UINT m_is_init:1;

    INT m_last_idx;    //Last element idx
public:
    T * m_vec;

    Vector()
    {
        SVEC_init(this) = false;
        init();
    }

    explicit Vector(INT size)
    {
        SVEC_init(this) = false;
        init();
        grow(size);
    }

    Vector(Vector const& vec) { copy(vec); }
    Vector const& operator = (Vector const&);
    ~Vector() { destroy();    }

    void add(T t)
    {
        ASSERT(is_init(), ("VECTOR not yet initialized."));
        set(m_last_idx < 0 ? 0 : m_last_idx, t);
    }

    inline void init()
    {
        if (SVEC_init(this)) { return; }
        m_elem_num = 0;
        m_vec = NULL;
        m_last_idx = -1;
        SVEC_init(this) = true;
    }

    inline void init(UINT size)
    {
        if (SVEC_init(this)) { return; }
        ASSERT0(size != 0);
        m_vec = (T*)::malloc(sizeof(T) * size);
        ASSERT0(m_vec);
        ::memset(m_vec, 0, sizeof(T) * size);
        m_elem_num = size;
        m_last_idx = -1;
        SVEC_init(this) = true;
    }

    bool is_init() const { return SVEC_init(this); }

    inline void destroy()
    {
        if (!SVEC_init(this)) { return; }
        m_elem_num = 0;
        if (m_vec != NULL) {
            ::free(m_vec);
        }
        m_vec = NULL;
        m_last_idx = -1;
        SVEC_init(this) = false;
    }

    //The function often invoked by destructor, to speed up destruction time.
    //Since reset other members is dispensable.
    void destroy_vec()
    {
        if (m_vec != NULL) {
            ::free(m_vec);
        }
    }

    T get(UINT index) const
    {
        ASSERT(is_init(), ("VECTOR not yet initialized."));
        return (index >= (UINT)m_elem_num) ? T(0) : m_vec[index];
    }

    T * get_vec() { return m_vec; }

    //Overloaded [] for CONST array reference create an rvalue.
    //Similar to 'get()', the difference between this operation
    //and get() is [] opeartion does not allow index is greater than
    //or equal to m_elem_num.
    //Note this operation can not be used to create lvalue.
    //e.g: Vector<int> const v;
    //    int ex = v[i];
    T const operator[](UINT index) const
    {
        ASSERT(is_init(), ("VECTOR not yet initialized."));
        ASSERT(index < (UINT)m_elem_num, ("array subscript over boundary."));
        return m_vec[index];
    }

    //Overloaded [] for non-const array reference create an lvalue.
    //Similar to set(), the difference between this operation
    //and set() is [] opeartion does not allow index is greater than
    //or equal to m_elem_num.
    inline T & operator[](UINT index)
    {
        ASSERT(is_init(), ("VECTOR not yet initialized."));
        ASSERT(index < (UINT)m_elem_num, ("array subscript over boundary."));
        m_last_idx = MAX((INT)index, m_last_idx);
        return m_vec[index];
    }

    //Copy each elements of 'list' into vector.
    //NOTE: The default termination factor is '0'.
    //    While we traversing elements of List one by one, or from head to
    //    tail or on opposition way, one can copy list into vector first and
    //    iterating the vector instead of travering list.
    void copy(List<T> & list)
    {
        INT count = 0;
        set(list.get_elem_count() - 1, 0); //Alloc memory right away.
        for (T elem = list.get_head();
             elem != T(0); elem = list.get_next(), count++) {
            set(count, elem);
        }
    }

    void copy(Vector const& vec)
    {
        ASSERT0(vec.m_elem_num > 0 ||
                 (vec.m_elem_num == 0 && vec.m_last_idx == -1));
        UINT n = vec.m_elem_num;
        if (m_elem_num < n) {
            destroy();
            init(n);
        }
        if (n > 0) {
            memcpy(m_vec, vec.m_vec, sizeof(T) * n);
        }
        m_last_idx = vec.m_last_idx;
    }

    //Clean to zero(default) till 'last_idx'.
    void clean()
    {
        ASSERT(is_init(), ("VECTOR not yet initialized."));
        if (m_last_idx < 0) {
            return;
        }
        ::memset(m_vec, 0, sizeof(T) * (m_last_idx + 1));
        m_last_idx = -1; //No any elements
    }

    UINT count_mem() const { return m_elem_num * sizeof(T) + sizeof(Vector<T>); }

    //Place elem to vector according to index.
    //Growing vector if 'index' is greater than m_elem_num.
    void set(UINT index, T elem)
    {
        ASSERT(is_init(), ("VECTOR not yet initialized."));
        if (index >= (UINT)m_elem_num) {
            grow(getNearestPowerOf2(index) + 2);
        }
        m_last_idx = MAX((INT)index, m_last_idx);
        m_vec[index] = elem;
        return;
    }

    //Return the number of element the vector could hold.
    UINT get_capacity() const
    {
        ASSERT(is_init(), ("VECTOR not yet initialized."));
        return m_elem_num;
    }

    INT get_last_idx() const
    {
        ASSERT(is_init(), ("VECTOR not yet initialized."));
        ASSERT(m_last_idx < (INT)m_elem_num,
                ("Last index ran over Vector's size."));
        return m_last_idx;
    }

    //Growing vector up to hold the most num_of_elem elements.
    //If 'm_elem_num' is 0 , alloc vector to hold num_of_elem elements.
    //Reallocate memory if necessory.
    void grow(UINT num_of_elem)
    {
        ASSERT(is_init(), ("VECTOR not yet initialized."));
        if (num_of_elem == 0) { return; }
        if (m_elem_num == 0) {
            ASSERT(m_vec == NULL, ("vector should be NULL if size is zero."));
            m_vec = (T*)::malloc(sizeof(T) * num_of_elem);
            ASSERT0(m_vec);
            memset(m_vec, 0, sizeof(T) * num_of_elem);
            m_elem_num = num_of_elem;
            return;
        }

        ASSERT0(num_of_elem > (UINT)m_elem_num);
        T * tmp = (T*)::malloc(num_of_elem * sizeof(T));
        ASSERT0(tmp);
        memcpy(tmp, m_vec, m_elem_num * sizeof(T));
        memset(((CHAR*)tmp) + m_elem_num * sizeof(T), 0,
               (num_of_elem - m_elem_num)* sizeof(T));
        ::free(m_vec);
        m_vec = tmp;
        m_elem_num = num_of_elem;
    }

    //Return true if there is not any element.
    bool is_empty() const { return get_last_idx() == -1; }
};
//END Vector



//The extented class to Vector.
//This class maintains an index which call Free Index of Vector.
//User can use the Free Index to get to know which slot of vector
//is T(0).
//
//e.g: assume Vector<INT> has 6 elements, {0, 3, 4, 0, 5, 0}.
//Three of them are 0, where T(0) served as default NULL and these
//elements indicate free slot.
//In the case, the first free index is 0, the second is 3, and the
//third is 5.
template <class T, UINT GrowSize = 8>
class VectorWithFreeIndex : public Vector<T, GrowSize> {
protected:
    //Always refers to free space to Vector,
    //and the first free space position is '0'
    UINT m_free_idx;
public:
    VectorWithFreeIndex()
    {
        SVEC_init(this) = false;
        init();
    }
    COPY_CONSTRUCTOR(VectorWithFreeIndex);
    inline void init()
    {
        Vector<T, GrowSize>::init();
        m_free_idx = 0;
    }

    inline void init(UINT size)
    {
        Vector<T, GrowSize>::init(size);
        m_free_idx = 0;
    }

    void copy(VectorWithFreeIndex const& vec)
    {
        Vector<T, GrowSize>::copy(vec);
        m_free_idx = vec.m_free_idx;
    }

    //Clean to zero(default) till 'last_idx'.
    void clean()
    {
        Vector<T, GrowSize>::clean();
        m_free_idx = 0;
    }

    //Return index of free-slot into Vector, and allocate memory
    //if there are not any free-slots.
    //
    //v: default NULL value.
    //
    //NOTICE:
    //    The condition that we considered a slot is free means the
    //    value of that slot is equal to v.
    UINT get_free_idx(T v = T(0))
    {
        ASSERT((Vector<T, GrowSize>::is_init()),
                ("VECTOR not yet initialized."));
        if (Vector<T, GrowSize>::m_elem_num == 0) {
            //VECTOR is empty.
            ASSERT((Vector<T, GrowSize>::m_last_idx < 0 &&
                    Vector<T, GrowSize>::m_vec == NULL),
                   ("exception occur in Vector"));
            grow(GrowSize);

            //Free space is started at position '0',
            //so next free space is position '1'.
            m_free_idx = 1;
            return 0;
        }

        //VECTOR is not empty.
        UINT ret = m_free_idx;

        //Seaching in second-half Vector to find the next free idx.
        for (UINT i = m_free_idx + 1; i < Vector<T, GrowSize>::m_elem_num; i++) {
            if (v == Vector<T, GrowSize>::m_vec[i]) {
                m_free_idx = i;
                return ret;
            }
        }

        //Seaching in first-half Vector to find the next free idx.
        for (UINT i = 0; i < m_free_idx; i++) {
            if (v == Vector<T, GrowSize>::m_vec[i]) {
                m_free_idx = i;
                return ret;
            }
        }

        m_free_idx = Vector<T, GrowSize>::m_elem_num;

        grow(Vector<T, GrowSize>::m_elem_num * 2);

        return ret;
    }

    //Growing vector by size, if 'm_size' is NULL , alloc vector.
    //Reallocate memory if necessory.
    void grow(UINT size)
    {
        if (Vector<T, GrowSize>::m_elem_num == 0) {
            m_free_idx = 0;
        }
        Vector<T, GrowSize>::grow(size);
    }
};



//Simply Vector.
//
//T: refer to element type.
//NOTE: Zero is treated as the default NULL when we
//determine the element existence.
#define SVEC_elem_num(s)        ((s)->s1.m_elem_num)
template <class T, UINT GrowSize> class SimpleVec {
protected:
    struct {
        UINT m_elem_num:31;    //The number of element in vector.
        UINT m_is_init:1;
    } s1;
public:
    T * m_vec;

public:
    SimpleVec()
    {
        s1.m_is_init = false;
        init();
    }
    explicit SimpleVec(INT size)
    {
        s1.m_is_init = false;
        init(size);
    }
    SimpleVec(SimpleVec const& src) { copy(src); }
    SimpleVec const& operator = (SimpleVec const&);
    ~SimpleVec() { destroy(); }

    inline void init()
    {
        if (s1.m_is_init) { return; }
        SVEC_elem_num(this) = 0;
        m_vec = NULL;
        s1.m_is_init = true;
    }

    inline void init(UINT size)
    {
        if (s1.m_is_init) { return; }
        ASSERT0(size != 0);
        m_vec = (T*)::malloc(sizeof(T) * size);
        ASSERT0(m_vec);
        ::memset(m_vec, 0, sizeof(T) * size);
        SVEC_elem_num(this) = size;
        s1.m_is_init = true;
    }

    inline void destroy()
    {
        if (!s1.m_is_init) { return; }
        SVEC_elem_num(this) = 0;
        if (m_vec != NULL) {
            ::free(m_vec);
            m_vec = NULL;
        }
        s1.m_is_init = false;
    }

    //The function often invoked by destructor, to speed up destruction time.
    //Since reset other members is dispensable.
    void destroy_vec()
    {
        if (m_vec != NULL) {
            ::free(m_vec);
            m_vec = NULL;
            SVEC_elem_num(this) = 0;
        }
    }

    T get(UINT i) const
    {
        ASSERT(s1.m_is_init, ("SimpleVec not yet initialized."));
        if (i >= SVEC_elem_num(this)) { return T(0); }
        return m_vec[i];
    }

    T * get_vec() { return m_vec; }

    void copy(SimpleVec const& vec)
    {
        UINT n = SVEC_elem_num(&vec);
        if (SVEC_elem_num(this) < n) {
            destroy();
            init(n);
        }
        if (n > 0) {
            ::memcpy(m_vec, vec.m_vec, sizeof(T) * n);
        }
    }

    //Clean to zero(default) till 'last_idx'.
    void clean()
    {
        ASSERT(s1.m_is_init, ("SimpleVec not yet initialized."));
        memset(m_vec, 0, sizeof(T) * SVEC_elem_num(this));
    }

    UINT count_mem() const { return SVEC_elem_num(this) + sizeof(Vector<T>); }

    //Return NULL if 'i' is illegal, otherwise return 'elem'.
    void set(UINT i, T elem)
    {
        ASSERT(is_init(), ("SimpleVec not yet initialized."));
        if (i >= SVEC_elem_num(this)) {
            grow(i * 2 + 1);
        }
        m_vec[i] = elem;
        return;
    }

    //Return the number of element the vector could hold.
    UINT get_capacity() const
    {
        ASSERT(is_init(), ("SimpleVec not yet initialized."));
        return SVEC_elem_num(this);
    }

    //Growing vector by size, if 'm_size' is NULL , alloc vector.
    //Reallocate memory if necessory.
    void grow(UINT size)
    {
        ASSERT(is_init(), ("SimpleVec not yet initialized."));
        if (size == 0) { return; }
        if (SVEC_elem_num(this) == 0) {
            ASSERT(m_vec == NULL,
                   ("SimpleVec should be NULL if size is zero."));
            m_vec = (T*)::malloc(sizeof(T) * size);
            ASSERT0(m_vec);
            memset(m_vec, 0, sizeof(T) * size);
            SVEC_elem_num(this) = size;
            return;
        }

        ASSERT0(size > SVEC_elem_num(this));
        T * tmp = (T*)::malloc(size * sizeof(T));
        ASSERT0(tmp);
        memcpy(tmp, m_vec, SVEC_elem_num(this) * sizeof(T));
        memset(((CHAR*)tmp) + SVEC_elem_num(this) * sizeof(T),
               0, (size - SVEC_elem_num(this))* sizeof(T));
        ::free(m_vec);
        m_vec = tmp;
        SVEC_elem_num(this) = size;
    }

    bool is_init() const { return s1.m_is_init; }
};



//Hash
//
//Hash element recorded not only in hash table but also in Vector,
//which is used in order to speed up accessing hashed elements.
//
//NOTICE:
//    1.T(0) is defined as default NULL in Hash, so do not use T(0) as element.
//
//    2.There are four hash function classes are given as default, and
//      if you are going to define you own hash function class, the
//      following member functions you should supply according to your needs.
//
//        * Return hash-key deduced from 'val'.
//            UINT get_hash_value(OBJTY val) const
//
//        * Return hash-key deduced from 't'.
//            UINT get_hash_value(T * t) const
//
//        * Compare t1, t2 when inserting a new element.
//            bool compare(T * t1, T * t2) const
//
//        * Compare t1, val when inserting a new element.
//            bool compare(T * t1, OBJTY val) const
//
//    3.Use 'new'/'delete' operator to allocate/free the memory
//      of dynamic object and the virtual function pointers.
#define HC_val(c)            (c)->val
#define HC_vec_idx(c)        (c)->vec_idx
#define HC_next(c)            (c)->next
#define HC_prev(c)            (c)->prev
template <class T> struct HC {
    HC<T> * prev;
    HC<T> * next;
    UINT vec_idx;
    T val;
};

#define HB_member(hm)        (hm).hash_member
#define HB_count(hm)        (hm).hash_member_count
class HashBucket {
public:
    void * hash_member; //hash member list
    UINT hash_member_count; //
};


template <class T> class HashFuncBase {
public:
    UINT get_hash_value(T t, UINT bucket_size) const
    {
        ASSERT0(bucket_size != 0);
        return ((UINT)(size_t)t) % bucket_size;
    }

    UINT get_hash_value(OBJTY val, UINT bucket_size) const
    {
        ASSERT0(bucket_size != 0);
        return ((UINT)(size_t)val) % bucket_size;
    }

    bool compare(T t1, T t2) const
    { return t1 == t2; }

    bool compare(T t1, OBJTY val) const
    { return t1 == (T)val; }
};


template <class T> class HashFuncBase2 : public HashFuncBase<T> {
public:
    UINT get_hash_value(T t, UINT bucket_size) const
    {
        ASSERT0(bucket_size != 0);
        ASSERT0(isPowerOf2(bucket_size));
        return hash32bit((UINT)(size_t)t) & (bucket_size - 1);
    }

    UINT get_hash_value(OBJTY val, UINT bucket_size) const
    {
        ASSERT0(bucket_size != 0);
        ASSERT0(isPowerOf2(bucket_size));
        return hash32bit((UINT)(size_t)val) & (bucket_size - 1);
    }
};


class HashFuncString {
public:
    UINT get_hash_value(CHAR const* s, UINT bucket_size) const
    {
        ASSERT0(bucket_size != 0);
        UINT v = 0;
        while (*s++) {
            v += (UINT)(*s);
        }
        return hash32bit(v) % bucket_size;
    }

    UINT get_hash_value(OBJTY v, UINT bucket_size) const
    {
        ASSERT_UNUSED(sizeof(OBJTY) == sizeof(CHAR*),
            ("exception will taken place in type-cast"));
        return get_hash_value((CHAR const*)v, bucket_size);
    }

    bool compare(CHAR const* s1, CHAR const* s2) const
    { return strcmp(s1, s2) == 0; }

    bool compare(CHAR const* s, OBJTY val) const
    {
        ASSERT_UNUSED(sizeof(OBJTY) == sizeof(CHAR const*),
            ("exception will taken place in type-cast"));
        return (strcmp(s,  (CHAR const*)val) == 0);
    }
};


class HashFuncString2 : public HashFuncString {
public:
    UINT get_hash_value(CHAR const* s, UINT bucket_size) const
    {
        ASSERT0(bucket_size != 0);
        UINT v = 0;
        while (*s++) {
            v += (UINT)(*s);
        }
        ASSERT0(isPowerOf2(bucket_size));
        return hash32bit(v) & (bucket_size - 1);
    }
};


//'T': the element type.
//'HF': Hash function type.
template <class T, class HF = HashFuncBase<T> > class Hash {
protected:
    HF m_hf;
    SMemPool * m_free_list_pool;
    FreeList<HC<T> > m_free_list; //Hold for available containers
    UINT m_bucket_size;
    HashBucket * m_bucket;
    VectorWithFreeIndex<T, 8> m_elem_vector;
    UINT m_elem_count;

    inline HC<T> * newhc() //Allocate hash container.
    {
        ASSERT(m_bucket != NULL, ("HASH not yet initialized."));
        ASSERT0(m_free_list_pool);
        HC<T> * c = m_free_list.get_free_elem();
        if (c == NULL) {
            c = (HC<T>*)smpoolMallocConstSize(sizeof(HC<T>), m_free_list_pool);
            ASSERT0(c);
            memset(c, 0, sizeof(HC<T>));
        }
        return c;
    }

    //Insert element into hash table.
    //Return true if 't' already exist.
    inline bool insert_v(OUT HC<T> ** bucket_entry, OUT HC<T> ** hc, OBJTY val)
    {
        HC<T> * elemhc = *bucket_entry;
        HC<T> * prev = NULL;
        while (elemhc != NULL) {
            ASSERT(HC_val(elemhc) != T(0),
                   ("Hash element has so far as to be overrided!"));
            if (m_hf.compare(HC_val(elemhc), val)) {
                *hc = elemhc;
                return true;
            }
            prev = elemhc;
            elemhc = HC_next(elemhc);
        } //end while
        elemhc = newhc();

        ASSERT(elemhc, ("newhc() return NULL"));
        HC_val(elemhc) = create(val);
        if (prev != NULL) {
            //Append on element-list
            HC_next(prev) = elemhc;
            HC_prev(elemhc) = prev;
        } else {
            *bucket_entry = elemhc;
        }
        *hc = elemhc;
        return false;
    }

    //Insert element into hash table.
    //Return true if 't' already exist.
    inline bool insert_t(IN OUT HC<T> ** bucket_entry, OUT HC<T> ** hc, IN T t)
    {
        HC<T> * prev = NULL;
        HC<T> * elemhc = *bucket_entry;
        while (elemhc != NULL) {
            ASSERT(HC_val(elemhc) != T(0), ("Container is empty"));
            if (m_hf.compare(HC_val(elemhc), t)) {
                t = HC_val(elemhc);
                *hc = elemhc;
                return true;
            }
            prev = elemhc;
            elemhc = HC_next(elemhc);
        }

        elemhc = newhc();

        ASSERT(elemhc, ("newhc() return NULL"));
        HC_val(elemhc) = t;
        if (prev != NULL) {
            //Append on elem-list in the bucket.
            HC_next(prev) = elemhc;
            HC_prev(elemhc) = prev;
        } else {
            *bucket_entry = elemhc;
        }
        *hc = elemhc;
        return false;
    }

    virtual T create(OBJTY v)
    {
        ASSERT(0, ("Inherited class need to implement"));
        UNUSED(v);
        return T(0);
    }
public:
    Hash(UINT bsize = MAX_SHASH_BUCKET)
    {
        m_bucket = NULL;
        m_bucket_size = 0;
        m_free_list_pool = NULL;
        m_elem_count = 0;
        init(bsize);
    }
    COPY_CONSTRUCTOR(Hash);
    virtual ~Hash() { destroy(); }

    //Append 't' into hash table and record its reference into
    //Vector in order to walk through the table rapidly.
    //If 't' already exists, return the element immediately.
    //'find': set to true if 't' already exist.
    //
    //NOTE:
    //    Do NOT append 0 to table.
    //    Maximum size of T equals sizeof(void*).
    T append(T t, OUT HC<T> ** hct = NULL, bool * find = NULL)
    {
        ASSERT(m_bucket != NULL, ("Hash not yet initialized."));
        if (t == T(0)) return T(0);

        UINT hashv = m_hf.get_hash_value(t, m_bucket_size);
        ASSERT(hashv < m_bucket_size,
               ("hash value must less than bucket size"));

        HC<T> * elemhc = NULL;
        if (!insert_t((HC<T>**)&HB_member(m_bucket[hashv]), &elemhc, t)) {
            HB_count(m_bucket[hashv])++;
            m_elem_count++;

            //Get a free slot in elem-vector
            HC_vec_idx(elemhc) = m_elem_vector.get_free_idx();
            m_elem_vector.set(HC_vec_idx(elemhc), t);
            if (find != NULL) {
                *find = false;
            }
        } else if (find != NULL) {
            *find = true;
        }

        if (hct != NULL) {
            *hct = elemhc;
        }
        return HC_val(elemhc);
    }

    //Append 'val' into hash table.
    //More comment see above function.
    //'find': set to true if 't' already exist.
    //
    //NOTE: Do NOT append T(0) to table.
    T append(OBJTY val, OUT HC<T> ** hct = NULL, bool * find = NULL)
    {
        ASSERT(m_bucket != NULL, ("Hash not yet initialized."));
        UINT hashv = m_hf.get_hash_value(val, m_bucket_size);

        ASSERT(hashv < m_bucket_size, ("hash value must less than bucket size"));
        HC<T> * elemhc = NULL;
        if (!insert_v((HC<T>**)&HB_member(m_bucket[hashv]), &elemhc, val)) {
            HB_count(m_bucket[hashv])++;
            m_elem_count++;

            //Get a free slot in elem-vector
            HC_vec_idx(elemhc) = m_elem_vector.get_free_idx();
            m_elem_vector.set(HC_vec_idx(elemhc), HC_val(elemhc));
            if (find != NULL) {
                *find = false;
            }
        } else if (find != NULL) {
            *find = true;
        }

        if (hct != NULL) {
            *hct = elemhc;
        }
        return HC_val(elemhc);
    }

    //Count up the memory which hash table used.
    size_t count_mem() const
    {
        size_t count = smpoolGetPoolSize(m_free_list_pool);
        count += m_free_list.count_mem();
        count += sizeof(m_bucket_size);
        count += sizeof(m_bucket);
        count += m_bucket_size;
        count += m_elem_vector.count_mem();
        count += sizeof(m_elem_count);
        return count;
    }

    //Clean the data structure but not destroy.
    void clean()
    {
        if (m_bucket == NULL) return;
        memset(m_bucket, 0, sizeof(HashBucket) * m_bucket_size);
        m_elem_count = 0;
        m_elem_vector.clean();
    }

    //Get the hash bucket size.
    UINT get_bucket_size() const { return m_bucket_size; }

    //Get the hash bucket.
    HashBucket const* get_bucket() const { return m_bucket; }

    //Get the memory pool handler of free_list.
    //Note this pool is const size.
    SMemPool * get_free_list_pool() const { return m_free_list_pool; };

    //Get the number of element in hash table.
    UINT get_elem_count() const { return m_elem_count; }

    //This function return the first element if it exists, and initialize
    //the iterator, otherwise return T(0), where T is the template parameter.
    //
    //When T is type of integer, return zero may be fuzzy and ambiguous.
    //Invoke get_next() to get the next element.
    //
    //'iter': iterator, when the function return, cur will be updated.
    //    If the first element exist, cur is a value that great and equal 0,
    //    or is -1.
    T get_first(INT & iter) const
    {
        ASSERT(m_bucket != NULL, ("Hash not yet initialized."));
        T t = T(0);
        iter = -1;
        if (m_elem_count <= 0) return T(0);
        INT l = m_elem_vector.get_last_idx();
        for (INT i = 0; i <= l; i++) {
            if ((t = m_elem_vector.get((UINT)i)) != T(0)) {
                iter = i;
                return t;
            }
        }
        return T(0);
    }

    //This function return the next element of given iterator.
    //If it exists, record its index at 'iter' and return the element,
    //otherwise set 'iter' to -1, and return T(0), where T is the
    //template parameter.
    //
    //'iter': iterator, when the function return, cur will be updated.
    //    If the first element exist, cur is a value that great and equal 0,
    //    or is -1.
    T get_next(INT & iter) const
    {
        ASSERT(m_bucket != NULL && iter >= -1, ("Hash not yet initialized."));
        T t = T(0);
        if (m_elem_count <= 0) return T(0);
        INT l = m_elem_vector.get_last_idx();
        for (INT i = iter + 1; i <= l; i++) {
            if ((t = m_elem_vector.get((UINT)i)) != T(0)) {
                iter = i;
                return t;
            }
        }
        iter = -1;
        return T(0);
    }

    //This function return the last element if it exists, and initialize
    //the iterator, otherwise return T(0), where T is the template parameter.
    //When T is type of integer, return zero may be fuzzy and ambiguous.
    //Invoke get_prev() to get the prev element.
    //
    //'iter': iterator, when the function return, cur will be updated.
    //    If the first element exist, cur is a value that great and equal 0,
    //    or is -1.
    T get_last(INT & iter) const
    {
        ASSERT(m_bucket != NULL, ("Hash not yet initialized."));
        T t = T(0);
        iter = -1;
        if (m_elem_count <= 0) return T(0);
        INT l = m_elem_vector.get_last_idx();
        for (INT i = l; i >= 0; i--) {
            if ((t = m_elem_vector.get((UINT)i)) != T(0)) {
                iter = i;
                return t;
            }
        }
        return T(0);
    }

    //This function return the previous element of given iterator.
    //If it exists, record its index at 'iter' and return the element,
    //otherwise set 'iter' to -1, and return T(0), where T is the
    //template parameter.
    //
    //'iter': iterator, when the function return, cur will be updated.
    //    If the first element exist, cur is a value that great and equal 0,
    //    or is -1.
    T get_prev(INT & iter) const
    {
        ASSERT(m_bucket != NULL, ("Hash not yet initialized."));
        T t = T(0);
        if (m_elem_count <= 0) return T(0);
        for (INT i = iter - 1; i >= 0; i--) {
            if ((t = m_elem_vector.get((UINT)i)) != T(0)) {
                iter = i;
                return t;
            }
        }
        iter = -1;
        return T(0);
    }

    void init(UINT bsize = MAX_SHASH_BUCKET)
    {
        if (m_bucket != NULL || bsize == 0) { return; }
        m_bucket = (HashBucket*)::malloc(sizeof(HashBucket) * bsize);
        memset(m_bucket, 0, sizeof(HashBucket) * bsize);
        m_bucket_size = bsize;
        m_elem_count = 0;
        m_free_list_pool = smpoolCreate(sizeof(HC<T>) * 4, MEM_CONST_SIZE);
        m_free_list.clean();
        m_free_list.set_clean(true);
        m_elem_vector.init();
    }

    //Free all memory objects.
    void destroy()
    {
        if (m_bucket == NULL) return;
        ::free(m_bucket);
        m_bucket = NULL;
        m_bucket_size = 0;
        m_elem_count = 0;
        m_elem_vector.destroy();
        smpoolDelete(m_free_list_pool);
        m_free_list_pool = NULL;
    }

    //Dump the distribution of element in hash.
    void dump_intersp(FILE * h) const
    {
        if (h == NULL) return;
        UINT bsize = get_bucket_size();
        HashBucket const* bucket = get_bucket();
        fprintf(h, "\n=== Hash ===");
        for (UINT i = 0; i < bsize; i++) {
            HC<T> * elemhc = (HC<T>*)bucket[i].hash_member;
            fprintf(h, "\nENTRY[%d]:", i);
            while (elemhc != NULL) {
                fprintf(h, "*");
                elemhc = HC_next(elemhc);
                if (elemhc != NULL) {
                    //fprintf(g_tfile, ",");
                }
            }
        }
        fflush(h);
    }

    //This function remove one element, and return the removed one.
    //Note that 't' may be different with the return one accroding to
    //the behavior of user's defined HF class.
    //
    //Do NOT change the order that elements in m_elem_vector and the
    //value of m_cur. Because it will impact the effect of get_first(),
    //get_next(), get_last() and get_prev().
    T removed(T t)
    {
        ASSERT(m_bucket != NULL, ("Hash not yet initialized."));
        if (t == 0) return T(0);

        UINT hashv = m_hf.get_hash_value(t, m_bucket_size);
        ASSERT(hashv < m_bucket_size,
                ("hash value must less than bucket size"));
        HC<T> * elemhc = (HC<T>*)HB_member(m_bucket[hashv]);
        if (elemhc != NULL) {
            while (elemhc != NULL) {
                ASSERT(HC_val(elemhc) != T(0),
                        ("Hash element has so far as to be overrided!"));
                if (m_hf.compare(HC_val(elemhc), t)) {
                    break;
                }
                elemhc = HC_next(elemhc);
            }
            if (elemhc != NULL) {
                m_elem_vector.set(HC_vec_idx(elemhc), T(0));
                remove((HC<T>**)&HB_member(m_bucket[hashv]), elemhc);
                m_free_list.add_free_elem(elemhc);
                HB_count(m_bucket[hashv])--;
                m_elem_count--;
                return t;
            }
        }
        return T(0);
    }

    //Grow hash to 'bsize' and rehash all elements in the table.
    //The default grow size is twice as the current bucket size.
    //
    //'bsize': expected bucket size, the size can not less than current size.
    //
    //NOTE: Extending hash table is costly.
    //The position of element in m_elem_vector is unchanged.
    void grow(UINT bsize = 0)
    {
        ASSERT(m_bucket != NULL, ("Hash not yet initialized."));
        if (bsize != 0) {
            ASSERT0(bsize > m_bucket_size);
        } else {
            bsize = m_bucket_size * 2;
        }

        HashBucket * new_bucket =
            (HashBucket*)::malloc(sizeof(HashBucket) * bsize);
        memset(new_bucket, 0, sizeof(HashBucket) * bsize);
        if (m_elem_count == 0) {
            ::free(m_bucket);
            m_bucket = new_bucket;
            m_bucket_size = bsize;
            return;
        }

        //Free HC containers.
        for (UINT i = 0; i < m_bucket_size; i++) {
            HC<T> * hc = NULL;
            while ((hc = xcom::removehead((HC<T>**)&HB_member(m_bucket[i]))) != NULL) {
                m_free_list.add_free_elem(hc);
            }
        }

        //Free bucket.
        ::free(m_bucket);

        m_bucket = new_bucket;
        m_bucket_size = bsize;

        //Rehash all elements, and the position in m_elem_vector is unchanged.
        INT l = m_elem_vector.get_last_idx();
        for (INT i = 0; i <= l; i++) {
            T t = m_elem_vector.get((UINT)i);
            if (t == T(0)) { continue; }

            UINT hashv = m_hf.get_hash_value(t, m_bucket_size);
            ASSERT(hashv < m_bucket_size,
                    ("hash value must less than bucket size"));

            HC<T> * elemhc = NULL;
            bool doit = insert_t((HC<T>**)&HB_member(m_bucket[hashv]),
                                 &elemhc, t);
            ASSERT0(!doit);
            UNUSED(doit); //to avoid -Werror=unused-variable.

            HC_vec_idx(elemhc) = (UINT)i;

            HB_count(m_bucket[hashv])++;
        }
    }

    //Find element accroding to specific 'val'.
    //You can implement your own find(), but do NOT
    //change the order that elements in m_elem_vector and the value of m_cur.
    //Because it will impact the effect of get_first(), get_next(),
    //get_last() and get_prev().
    T find(OBJTY val) const
    {
        ASSERT(m_bucket != NULL, ("Hash not yet initialized."));
        UINT hashv = m_hf.get_hash_value(val, m_bucket_size);
        ASSERT(hashv < m_bucket_size, ("hash value must less than bucket size"));
        HC<T> const* elemhc = (HC<T> const*)HB_member(m_bucket[hashv]);
        if (elemhc != NULL) {
            while (elemhc != NULL) {
                ASSERT(HC_val(elemhc) != T(0),
                       ("Hash element has so far as to be overrided!"));
                if (m_hf.compare(HC_val(elemhc), val)) {
                    return HC_val(elemhc);
                }
                elemhc = HC_next(elemhc);
            }
        }
        return T(0);
    }

    //Find one element and return the container.
    //Return true if t exist, otherwise return false.
    //You can implement your own find(), but do NOT
    //change the order that elements in m_elem_vector and the value of m_cur.
    //Because it will impact the effect of get_first(), get_next(),
    //get_last() and get_prev().
    bool find(T t, HC<T> const** ct = NULL) const
    {
        ASSERT(m_bucket != NULL, ("Hash not yet initialized."));
        if (t == T(0)) { return false; }

        UINT hashv = m_hf.get_hash_value(t, m_bucket_size);
        ASSERT(hashv < m_bucket_size,
               ("hash value must less than bucket size"));
        HC<T> const* elemhc = (HC<T> const*)HB_member(m_bucket[hashv]);
        if (elemhc != NULL) {
            while (elemhc != NULL) {
                ASSERT(HC_val(elemhc) != T(0),
                       ("Hash element has so far as to be overrided!"));
                if (m_hf.compare(HC_val(elemhc), t)) {
                    if (ct != NULL) {
                        *ct = elemhc;
                    }
                    return true;
                }
                elemhc = HC_next(elemhc);
            }
        }
        return false;
    }

    //Find one element and return the element which record in hash table.
    //Note t may be different with the return one.
    //
    //You can implement your own find(), but do NOT
    //change the order that elements in m_elem_vector and the value of m_cur.
    //Because it will impact the effect of get_first(), get_next(),
    //get_last() and get_prev().
    //
    //'ot': output the element if found it.
    bool find(T t, OUT T * ot) const
    {
        HC<T> const* hc;
        if (find(t, &hc)) {
            ASSERT0(ot != NULL);
            *ot = HC_val(hc);
            return true;
        }
        return false;
    }
};
//END Hash



//
//START RBTNode
//
typedef enum _RBT_RED {
    RBT_NON = 0,
    RBRED = 1,
    RBBLACK = 2,
} RBCOL;

template <class T, class Ttgt>
class RBTNode {
public:
    RBTNode * parent;
    union {
        RBTNode * lchild;
        RBTNode * prev;
    };

    union {
        RBTNode * rchild;
        RBTNode * next;
    };
    T key;
    Ttgt mapped;
    RBCOL color;

    RBTNode() { clean(); }

    void clean()
    {
        parent = NULL;
        lchild = NULL;
        rchild = NULL;
        key = T(0);
        mapped = Ttgt(0);
        color = RBBLACK;
    }
};


template <class T> class CompareKeyBase {
public:
    bool is_less(T t1, T t2) const { return t1 < t2; }
    bool is_equ(T t1, T t2) const { return t1 == t2; }
};

template <class T, class Ttgt, class CompareKey = CompareKeyBase<T> >
class RBT {
protected:
    typedef RBTNode<T, Ttgt> RBTNType;
    UINT m_num_of_tn;
    RBTNType * m_root;
    SMemPool * m_pool;
    RBTNType * m_free_list;
    CompareKey m_ck;

    RBTNType * new_tn()
    {
        RBTNType * p = (RBTNType*)smpoolMallocConstSize(sizeof(RBTNType), m_pool);
        ASSERT0(p);
        memset(p, 0, sizeof(RBTNType));
        return p;
    }

    inline RBTNType * new_tn(T t, RBCOL c)
    {
        RBTNType * x = xcom::removehead(&m_free_list);
        if (x == NULL) {
            x = new_tn();
        } else {
            x->lchild = NULL;
            x->rchild = NULL;
        }
        x->key = t;
        x->color = c;
        return x;
    }

    void free_rbt(RBTNType * t)
    {
        if (t == NULL) return;
        t->prev = t->next = t->parent = NULL;
        t->key = T(0);
        t->mapped = Ttgt(0);
        insertbefore_one(&m_free_list, m_free_list, t);
    }

    void rleft(RBTNType * x)
    {
        ASSERT0(x->rchild != NULL);
        RBTNType * y = x->rchild;
        y->parent = x->parent;
        x->parent = y;
        x->rchild = y->lchild;
        if (y->lchild != NULL) {
            y->lchild->parent = x;
        }
        y->lchild = x;
        if (y->parent == NULL) {
            m_root = y;
        } else if (y->parent->lchild == x) {
            y->parent->lchild = y;
        } else if (y->parent->rchild == x) {
            y->parent->rchild = y;
        }
    }

    void rright(RBTNType * y)
    {
        ASSERT0(y->lchild != NULL);
        RBTNType * x = y->lchild;
        x->parent = y->parent;
        y->parent = x;
        y->lchild = x->rchild;
        if (x->rchild != NULL) {
            x->rchild->parent = y;
        }
        x->rchild = y;
        if (x->parent == NULL) {
            m_root = x;
        } else if (x->parent->lchild == y) {
            x->parent->lchild = x;
        } else if (x->parent->rchild == y) {
            x->parent->rchild = x;
        }
    }

    bool is_lchild(RBTNType const* z) const
    {
        ASSERT0(z && z->parent);
        return z == z->parent->lchild;
    }

    bool is_rchild(RBTNType const* z) const
    {
        ASSERT0(z && z->parent);
        return z == z->parent->rchild;
    }

    void fixup(RBTNType * z)
    {
        RBTNType * y = NULL;
        while (z->parent != NULL && z->parent->color == RBRED) {
            if (is_lchild(z->parent)) {
                y = z->parent->parent->rchild;
                if (y != NULL && y->color == RBRED) {
                    z->parent->color = RBBLACK;
                    z->parent->parent->color = RBRED;
                    y->color = RBBLACK;
                    z = z->parent->parent;
                } else if (is_rchild(z)) {
                    z = z->parent;
                    rleft(z);
                } else {
                    ASSERT0(is_lchild(z));
                    z->parent->color = RBBLACK;
                    z->parent->parent->color = RBRED;
                    rright(z->parent->parent);
                }
            } else {
                ASSERT0(is_rchild(z->parent));
                y = z->parent->parent->lchild;
                if (y != NULL && y->color == RBRED) {
                    z->parent->color = RBBLACK;
                    z->parent->parent->color = RBRED;
                    y->color = RBBLACK;
                    z = z->parent->parent;
                } else if (is_lchild(z)) {
                    z = z->parent;
                    rright(z);
                } else {
                    ASSERT0(is_rchild(z));
                    z->parent->color = RBBLACK;
                    z->parent->parent->color = RBRED;
                    rleft(z->parent->parent);
                }
            }
        }
        m_root->color = RBBLACK;
    }
public:
    RBT()
    {
        m_pool = NULL;
        init();
    }
    COPY_CONSTRUCTOR(RBT);
    ~RBT() { destroy(); }

    void init()
    {
        if (m_pool != NULL) { return; }
        m_pool = smpoolCreate(sizeof(RBTNType) * 4, MEM_CONST_SIZE);
        m_root = NULL;
        m_num_of_tn = 0;
        m_free_list = NULL;
    }

    void destroy()
    {
        if (m_pool == NULL) { return; }
        smpoolDelete(m_pool);
        m_pool = NULL;
        m_num_of_tn = 0;
        m_root = NULL;
        m_free_list = NULL;
    }

    size_t count_mem() const
    {
        size_t c = sizeof(m_num_of_tn);
        c += sizeof(m_root);
        c += sizeof(m_pool);
        c += sizeof(m_free_list);
        c += smpoolGetPoolSize(m_pool);
        return c;
    }

    void clean()
    {
        List<RBTNType*> wl;
        if (m_root != NULL) {
            wl.append_tail(m_root);
            m_root = NULL;
        }
        while (wl.get_elem_count() != 0) {
            RBTNType * x = wl.remove_head();
            if (x->rchild != NULL) {
                wl.append_tail(x->rchild);
            }
            if (x->lchild != NULL) {
                wl.append_tail(x->lchild);
            }
            free_rbt(x);
        }
        m_num_of_tn = 0;
    }

    UINT get_elem_count() const { return m_num_of_tn; }
    SMemPool * get_pool() { return m_pool; }
    RBTNType * get_root() { return m_root; }

    RBTNType * find_with_key(T keyt) const
    {
        if (m_root == NULL) { return NULL; }
        RBTNType * x = m_root;
        while (x != NULL) {
            if (m_ck.is_equ(keyt, x->key)) {
                return x;
            } else if (m_ck.is_less(keyt, x->key)) {
                x = x->lchild;
            } else {
                x = x->rchild;
            }
        }
        return NULL;
    }

    inline RBTNType * find_rbtn(T t) const
    {
        if (m_root == NULL) { return NULL; }
        return find_with_key(t);
    }

    RBTNType * insert(T t, bool * find = NULL)
    {
        if (m_root == NULL) {
            RBTNType * z = new_tn(t, RBBLACK);
            m_num_of_tn++;
            m_root = z;
            if (find != NULL) {
                *find = false;
            }
            return z;
        }

        RBTNType * mark = NULL;
        RBTNType * x = m_root;
        while (x != NULL) {
            mark = x;
            if (m_ck.is_equ(t, x->key)) {
                break;
            } else if (m_ck.is_less(t, x->key)) {
                x = x->lchild;
            } else {
                x = x->rchild;
            }
        }

        if (x != NULL) {
            ASSERT0(m_ck.is_equ(t, x->key));
            if (find != NULL) {
                *find = true;
            }
            return x;
        }

        if (find != NULL) {
            *find = false;
        }

        //Add new.
        RBTNType * z = new_tn(t, RBRED);
        z->parent = mark;
        if (mark == NULL) {
            //The first node.
            m_root = z;
        } else {
            if (m_ck.is_less(t, mark->key)) {
                mark->lchild = z;
            } else {
                mark->rchild = z;
            }
        }
        ASSERT0(z->lchild == NULL && z->rchild == NULL);
        m_num_of_tn++;
        fixup(z);
        return z;
    }

    RBTNType * find_min(RBTNType * x)
    {
        ASSERT0(x);
        while (x->lchild != NULL) { x = x->lchild; }
        return x;
    }

    RBTNType * find_succ(RBTNType * x)
    {
        if (x->rchild != NULL) { return find_min(x->rchild); }
        RBTNType * y = x->parent;
        while (y != NULL && x == y->rchild) {
            x = y;
            y = y->parent;
        }
        return y;
    }

    inline bool both_child_black(RBTNType * x)
    {
        if (x->lchild != NULL && x->lchild->color == RBRED) {
            return false;
        }
        if (x->rchild != NULL && x->rchild->color == RBRED) {
            return false;
        }
        return true;
    }

    void rmfixup(RBTNType * x)
    {
        ASSERT0(x);
        while (x != m_root && x->color == RBBLACK) {
            if (is_lchild(x)) {
                RBTNType * bro = x->parent->rchild;
                ASSERT0(bro);
                if (bro->color == RBRED) {
                    bro->color = RBBLACK;
                    x->parent->color = RBRED;
                    rleft(x->parent);
                    bro = x->parent->rchild;
                }
                if (both_child_black(bro)) {
                    bro->color = RBRED;
                    x = x->parent;
                    continue;
                } else if (bro->rchild == NULL ||
                           bro->rchild->color == RBBLACK) {
                    ASSERT0(bro->lchild && bro->lchild->color == RBRED);
                    bro->lchild->color = RBBLACK;
                    bro->color = RBRED;
                    rright(bro);
                    bro = x->parent->rchild;
                }
                bro->color = x->parent->color;
                x->parent->color = RBBLACK;
                bro->rchild->color = RBBLACK;
                rleft(x->parent);
                x = m_root;
            } else {
                ASSERT0(is_rchild(x));
                RBTNType * bro = x->parent->lchild;
                if (bro->color == RBRED) {
                    bro->color = RBBLACK;
                    x->parent->color = RBRED;
                    rright(x->parent);
                    bro = x->parent->lchild;
                }
                if (both_child_black(bro)) {
                    bro->color = RBRED;
                    x = x->parent;
                    continue;
                } else if (bro->lchild == NULL ||
                           bro->lchild->color == RBBLACK) {
                    ASSERT0(bro->rchild && bro->rchild->color == RBRED);
                    bro->rchild->color = RBBLACK;
                    bro->color = RBRED;
                    rleft(bro);
                    bro = x->parent->lchild;
                }
                bro->color = x->parent->color;
                x->parent->color = RBBLACK;
                bro->lchild->color = RBBLACK;
                rright(x->parent);
                x = m_root;
            }
        }
        x->color = RBBLACK;
    }

    void remove(T t)
    {
        RBTNType * z = find_rbtn(t);
        if (z == NULL) { return; }
        remove(z);
    }

    void remove(RBTNType * z)
    {
        if (z == NULL) { return; }
        if (m_num_of_tn == 1) {
            ASSERT(z == m_root, ("z is not the node of tree"));
            ASSERT(z->rchild == NULL && z->lchild == NULL,
                    ("z is the last node"));
            free_rbt(z);
            m_num_of_tn--;
            m_root = NULL;
            return;
        }
        RBTNType * y;
        if (z->lchild == NULL || z->rchild == NULL) {
            y = z;
        } else {
            y = find_min(z->rchild);
        }

        RBTNType * x = y->lchild != NULL ? y->lchild : y->rchild;
        RBTNType holder;
        if (x != NULL) {
            x->parent = y->parent;
        } else {
            holder.parent = y->parent;
        }

        if (y->parent == NULL) {
            m_root = x;
        } else if (is_lchild(y)) {
            if (x != NULL) {
                y->parent->lchild = x;
            } else {
                y->parent->lchild = &holder;
            }
        } else {
            if (x != NULL) {
                y->parent->rchild = x;
            } else {
                y->parent->rchild = &holder;
            }
        }

        if (y != z) {
            z->key = y->key;
            z->mapped = y->mapped;
        }

        if (y->color == RBBLACK) {
            //Need to keep RB tree property: the number
            //of black must be same in all path.
            if (x != NULL) {
                rmfixup(x);
            } else {
                rmfixup(&holder);
                if (is_rchild(&holder)) {
                    holder.parent->rchild = NULL;
                } else {
                    ASSERT0(is_lchild(&holder));
                    holder.parent->lchild = NULL;
                }
            }
        } else if (holder.parent != NULL) {
            if (is_rchild(&holder)) {
                holder.parent->rchild = NULL;
            } else {
                ASSERT0(is_lchild(&holder));
                holder.parent->lchild = NULL;
            }
        }

        free_rbt(y);
        m_num_of_tn--;
    }

    //iter should be clean by caller.
    T get_first(List<RBTNType*> & iter, Ttgt * mapped = NULL) const
    {
        if (m_root == NULL) {
            if (mapped != NULL) { *mapped = Ttgt(0); }
            return T(0);
        }
        iter.append_tail(m_root);
        if (mapped != NULL) { *mapped = m_root->mapped; }
        return m_root->key;
    }

    T get_next(List<RBTNType*> & iter, Ttgt * mapped = NULL) const
    {
        RBTNType * x = iter.remove_head();
        if (x == NULL) {
            if (mapped != NULL) { *mapped = Ttgt(0); }
            return T(0);
        }
        if (x->rchild != NULL) {
            iter.append_tail(x->rchild);
        }
        if (x->lchild != NULL) {
            iter.append_tail(x->lchild);
        }
        x = iter.get_head();
        if (x == NULL) {
            if (mapped != NULL) { *mapped = Ttgt(0); }
            return T(0);
        }
        if (mapped != NULL) { *mapped = x->mapped; }
        return x->key;
    }
};
//END RBTNode



//TMap Iterator based on Double Linked List.
//This class is used to iterate elements in TMap.
//You should call clean() to initialize the iterator.
template <class Tsrc, class Ttgt>
class TMapIter : public List<RBTNode<Tsrc, Ttgt>*> {
public:
    TMapIter() {}
    COPY_CONSTRUCTOR(TMapIter);
};


//TMap Iterator based on Single Linked List.
//This class is used to iterate elements in TMap.
//You should call clean() to initialize the iterator.
template <class Tsrc, class Ttgt>
class TMapIter2 : public SList<RBTNode<Tsrc, Ttgt>*> {
public:
    typedef RBTNode<Tsrc, Ttgt>* Container;

public:
    TMapIter2(SMemPool * pool) : SList<RBTNode<Tsrc, Ttgt>*>(pool)
    { ASSERT0(pool); }
    COPY_CONSTRUCTOR(TMapIter2);
};


//TMap
//
//Make a map between Tsrc and Ttgt.
//
//Tsrc: the type of keys maintained by this map.
//Ttgt: the type of mapped values.
//
//Usage: Make a mapping from SRC* to TGT*.
//    class SRC2TGT_MAP : public TMap<SRC*, TGT*> {
//    public:
//    };
//
//NOTICE:
//    1. Tsrc(0) is defined as default NULL in TMap, do NOT use T(0) as element.
//    2. Keep the key *UNIQUE* .
//    3. Overload operator == and operator < if Tsrc is neither basic type
//       nor pointer type.
template <class Tsrc, class Ttgt, class CompareKey = CompareKeyBase<Tsrc> >
class TMap : public RBT<Tsrc, Ttgt, CompareKey> {
public:
    typedef RBT<Tsrc, Ttgt, CompareKey> BaseType;
    typedef RBTNode<Tsrc, Ttgt> RBTNType;
    TMap() {}
    COPY_CONSTRUCTOR(TMap);
    ~TMap() {}

    //This function should be invoked if TMap is initialized manually.
    void init() { RBT<Tsrc, Ttgt, CompareKey>::init(); }

    //This function should be invoked if TMap is destroied manually.
    void destroy() { RBT<Tsrc, Ttgt, CompareKey>::destroy(); }

    //Alway set new mapping even if it has done.
    //This function will enforce mapping between t and mapped.
    void setAlways(Tsrc t, Ttgt mapped)
    {
        RBTNType * z = BaseType::insert(t, NULL);
        ASSERT0(z);
        z->mapped = mapped;
    }

    //Establishing mapping in between 't' and 'mapped'.
    void set(Tsrc t, Ttgt mapped)
    {
        bool find = false;
        RBTNType * z = BaseType::insert(t, &find);
        ASSERT0(z);
        ASSERT(!find, ("already mapped"));
        z->mapped = mapped;
    }

    //Get mapped element of 't'. Set find to true if t is already be mapped.
    //Note this function is readonly.
    Ttgt get(Tsrc t, bool * f = NULL) const
    {
        RBTNType * z = BaseType::find_rbtn(t);
        if (z == NULL) {
            if (f != NULL) {
                *f = false;
            }
            return Ttgt(0);
        }

        if (f != NULL) {
            *f = true;
        }
        return z->mapped;
    }

    //iter should be clean by caller.
    Tsrc get_first(TMapIter<Tsrc, Ttgt> & iter, Ttgt * mapped = NULL) const
    { return BaseType::get_first(iter, mapped); }

    Tsrc get_next(TMapIter<Tsrc, Ttgt> & iter, Ttgt * mapped = NULL) const
    { return BaseType::get_next(iter, mapped); }

    bool find(Tsrc t) const
    {
        bool f;
        get(t, &f);
        return f;
    }

    void remove(Tsrc t) { BaseType::remove(t); }
};
//END TMap


//TTab
//
//NOTICE:
//    1. T(0) is defined as default NULL in TTab, do not use T(0) as element.
//    2. Keep the key *UNIQUE*.
//    3. Overload operator == and operator < if Tsrc is neither basic type
//       nor pointer type.
//
//    e.g: Make a table to record OPND.
//        class OPND_TAB : public TTab<OPND*> {
//        public:
//        };

//TTab Iterator.
//This class is used to iterate elements in TTab.
//You should call clean() to initialize the iterator.
template <class T>
class TabIter : public List<RBTNode<T, T>*> {
public:
    TabIter() {}
    COPY_CONSTRUCTOR(TabIter);
};

template <class T, class CompareKey = CompareKeyBase<T> >
class TTab : public TMap<T, T, CompareKey> {
public:
    TTab() {}
    COPY_CONSTRUCTOR(TTab);

    typedef RBT<T, T, CompareKey> BaseTypeofTMap;
    typedef TMap<T, T, CompareKey> BaseTMap;

    //Add element into table.
    //Note: the element in the table must be unqiue.
    void append(T t)
    {
        ASSERT0(t != T(0));
        #ifdef _DEBUG_
        bool find = false;
        T mapped = BaseTMap::get(t, &find);
        if (find) {
            ASSERT0(mapped == t);
        }
        #endif
        BaseTMap::setAlways(t, t);
    }

    //Add element into table, if it is exist, return the exist one.
    T append_and_retrieve(T t)
    {
        ASSERT0(t != T(0));

        bool find = false;
        T mapped = BaseTMap::get(t, &find);
        if (find) {
            return mapped;
        }

        BaseTMap::setAlways(t, t);
        return t;
    }

    void remove(T t)
    {
        ASSERT0(t != T(0));
        BaseTMap::remove(t);
    }

    bool find(T t) const { return BaseTMap::find(t); }

    //iter should be clean by caller.
    T get_first(TabIter<T> & iter) const
    { return BaseTypeofTMap::get_first(iter, NULL); }

    T get_next(TabIter<T> & iter) const
    { return BaseTypeofTMap::get_next(iter, NULL); }
};
//END TTab


//Unidirectional Hashed Map
//
//Tsrc: the type of keys maintained by this map.
//Ttgt: the type of mapped values.
//
//Usage: Make a mapping from OPND to OPER.
//    typedef HMap<OPND*, OPER*, HashFuncBase<OPND*> > OPND2OPER_MAP;
//
//NOTICE:
//    1. Tsrc(0) is defined as default NULL in HMap, so do not use T(0) as element.
//    2. The map is implemented base on Hash, and one hash function class
//       have been given.
//
//    3. Must use 'new'/'delete' operator to allocate/free the
//       memory of dynamic object of MAP, because the
//       virtual-function-pointers-table is needed.
template <class Tsrc, class Ttgt, class HF = HashFuncBase<Tsrc> >
class HMap : public Hash<Tsrc, HF> {
protected:
    Vector<Ttgt> m_mapped_elem_table;

    //Find hash container
    HC<Tsrc> * findhc(Tsrc t) const
    {
        if (t == Tsrc(0)) { return NULL; }
        UINT hashv = Hash<Tsrc, HF>::m_hf.get_hash_value(t,
                                Hash<Tsrc, HF>::m_bucket_size);
        ASSERT((hashv < Hash<Tsrc, HF>::m_bucket_size),
                ("hash value must less than bucket size"));
        HC<Tsrc> * elemhc =
            (HC<Tsrc>*)HB_member((Hash<Tsrc, HF>::m_bucket[hashv]));
        if (elemhc != NULL) {
            while (elemhc != NULL) {
                ASSERT(HC_val(elemhc) != Tsrc(0),
                        ("Hash element has so far as to be overrided!"));
                if (Hash<Tsrc, HF>::m_hf.compare(HC_val(elemhc), t)) {
                    return elemhc;
                }
                elemhc = HC_next(elemhc);
            } //end while
        }
        return NULL;
    }
public:
    HMap(UINT bsize = MAX_SHASH_BUCKET) : Hash<Tsrc, HF>(bsize)
    { m_mapped_elem_table.init(); }
    COPY_CONSTRUCTOR(HMap);
    virtual ~HMap() { destroy(); }

    //Alway set new mapping even if it has done.
    void setAlways(Tsrc t, Ttgt mapped)
    {
        ASSERT((Hash<Tsrc, HF>::m_bucket != NULL), ("not yet initialize."));
        if (t == Tsrc(0)) { return; }
        HC<Tsrc> * elemhc = NULL;
        Hash<Tsrc, HF>::append(t, &elemhc, NULL);
        ASSERT(elemhc != NULL, ("Element does not append into hash table."));
        m_mapped_elem_table.set(HC_vec_idx(elemhc), mapped);
    }

    SMemPool * get_pool() { return Hash<Tsrc, HF>::get_free_list_pool(); }

    //Get mapped pointer of 't'
    Ttgt get(Tsrc t, bool * find = NULL)
    {
        ASSERT((Hash<Tsrc, HF>::m_bucket != NULL), ("not yet initialize."));
        HC<Tsrc> * elemhc = findhc(t);
        if (elemhc != NULL) {
            if (find != NULL) { *find = true; }
            return m_mapped_elem_table.get(HC_vec_idx(elemhc));
        }
        if (find != NULL) { *find = false; }
        return Ttgt(0);
    }

    Vector<Ttgt> * get_tgt_elem_vec() { return &m_mapped_elem_table; }

    void clean()
    {
        ASSERT((Hash<Tsrc, HF>::m_bucket != NULL), ("not yet initialize."));
        Hash<Tsrc, HF>::clean();
        m_mapped_elem_table.clean();
    }

    UINT count_mem() const
    {
        UINT count = m_mapped_elem_table.count_mem();
        count += ((Hash<Tsrc, HF>*)this)->count_mem();
        return count;
    }

    void init(UINT bsize = MAX_SHASH_BUCKET)
    {
        //Only do initialization while m_bucket is NULL.
        Hash<Tsrc, HF>::init(bsize);
        m_mapped_elem_table.init();
    }

    void destroy()
    {
        Hash<Tsrc, HF>::destroy();
        m_mapped_elem_table.destroy();
    }

    //This function iterate mappped elements.
    //Note Ttgt(0) will be served as default NULL.
    Ttgt get_first_elem(INT & pos) const
    {
        for (INT i = 0; i <= m_mapped_elem_table.get_last_idx(); i++) {
            Ttgt t = m_mapped_elem_table.get((UINT)i);
            if (t != Ttgt(0)) {
                pos = i;
                return t;
            }
        }

        pos = -1;
        return Ttgt(0);
    }

    //This function iterate mappped elements.
    //Note Ttgt(0) will be served as default NULL.
    Ttgt get_next_elem(INT & pos) const
    {
        ASSERT0(pos >= 0);
        for (INT i = pos + 1; i <= m_mapped_elem_table.get_last_idx(); i++) {
            Ttgt t = m_mapped_elem_table.get((UINT)i);
            if (t != Ttgt(0)) {
                pos = i;
                return t;
            }
        }

        pos = -1;
        return Ttgt(0);

    }

    //Establishing mapping in between 't' and 'mapped'.
    void set(Tsrc t, Ttgt mapped)
    {
        ASSERT((Hash<Tsrc, HF>::m_bucket != NULL), ("not yet initialize."));
        if (t == Tsrc(0)) { return; }

        HC<Tsrc> * elemhc = NULL;
        Hash<Tsrc, HF>::append(t, &elemhc, NULL);

        ASSERT(elemhc != NULL,
               ("Element does not append into hash table yet."));
        ASSERT(Ttgt(0) == m_mapped_elem_table.get(HC_vec_idx(elemhc)),
               ("Already be mapped"));
        m_mapped_elem_table.set(HC_vec_idx(elemhc), mapped);
    }

    void setv(OBJTY v, Ttgt mapped)
    {
        ASSERT((Hash<Tsrc, HF>::m_bucket != NULL), ("not yet initialize."));
        if (v == 0) { return; }

        HC<Tsrc> * elemhc = NULL;
        Hash<Tsrc, HF>::append(v, &elemhc, NULL);
        ASSERT(elemhc != NULL,
               ("Element does not append into hash table yet."));
        ASSERT(Ttgt(0) == m_mapped_elem_table.get(HC_vec_idx(elemhc)),
               ("Already be mapped"));
        m_mapped_elem_table.set(HC_vec_idx(elemhc), mapped);
    }
};
//END MAP



//Dual directional Map
//
//MAP_Tsrc2Ttgt: class derive from HMap<Tsrc, Ttgt>
//MAP_Ttgt2Tsrc: class derive from HMap<Ttgt, Tsrc>
//
//Usage: Mapping OPND to corresponding OPER.
//    class MAP1 : public HMap<OPND*, OPER*> {
//    public:
//    };
//
//    class MAP2 : public HMap<OPER*, OPND*>{
//    public:
//    };
//
//    DMap<OPND*, OPER*, MAP1, MAP2> opnd2oper_dmap;
//
//NOTICE:
//    1. Tsrc(0) is defined as default NULL in DMap, so do not use T(0) as element.
//    2. DMap Object's memory can be allocated by malloc() dynamically.
template <class Tsrc, class Ttgt, class MAP_Tsrc2Ttgt, class MAP_Ttgt2Tsrc>
class DMap {
protected:
    MAP_Tsrc2Ttgt m_src2tgt_map;
    MAP_Ttgt2Tsrc m_tgt2src_map;
public:
    DMap() {}
    COPY_CONSTRUCTOR(DMap);
    ~DMap() {}

    //Alway overlap the old value by new 'mapped' value.
    void setAlways(Tsrc t, Ttgt mapped)
    {
        if (t == Tsrc(0)) { return; }
        m_src2tgt_map.setAlways(t, mapped);
        m_tgt2src_map.setAlways(mapped, t);
    }

    UINT count_mem() const
    {
        UINT count = m_src2tgt_map.count_mem();
        count += m_tgt2src_map.count_mem();
        return count;
    }

    //Establishing mapping in between 't' and 'mapped'.
    void set(Tsrc t, Ttgt mapped)
    {
        if (t == Tsrc(0)) { return; }
        m_src2tgt_map.set(t, mapped);
        m_tgt2src_map.set(mapped, t);
    }

    //Get mapped pointer of 't'
    Ttgt get(Tsrc t)
    { return m_src2tgt_map.get(t); }

    //Inverse mapping
    Tsrc geti(Ttgt t)
    { return m_tgt2src_map.get(t); }
};
//END DMap



//Multiple Target Map
//
//Map src with type Tsrc to many tgt elements with type Ttgt.
//
//'TAB_Ttgt': records tgt elements.
//    e.g: We implement TAB_Ttgt via deriving from Hash.
//
//    class TAB_Ttgt: public Hash<Ttgt, HashFuncBase<Ttgt> > {
//    public:
//    };
//
//    or deriving from List, typedef List<Ttgt> TAB_Ttgt;
//
//NOTICE:
//    1. Tsrc(0) is defined as default NULL in MMap, do not use T(0) as element.
//
//    2. MMap allocate memory for 'TAB_Ttgt' and return 'TAB_Ttgt *'
//        when get(Tsrc) be invoked. DO NOT free these objects yourself.
//
//    3. TAB_Ttgt should be pointer type.
//        e.g: Given type of tgt's table is a class that
//            OP_HASH : public Hash<OPER*>,
//            then type MMap<OPND*, OPER*, OP_HASH> is ok, but
//            type MMap<OPND*, OPER*, OP_HASH*> is not expected.
//
//    4. Do not use DMap directly, please overload following functions optionally:
//        * create hash-element container.
//            T * create(OBJTY v)
//
//        e.g: Mapping one OPND to a number of OPERs.
//            class OPND2OPER_MMAP : public MMap<OPND*, OPER*, OP_TAB> {
//            public:
//                virtual T create(OBJTY id)
//                {
//                    //Allocate OPND.
//                    return new OPND(id);
//                }
//            };
//
//    4. Please use 'new'/'delete' operator to allocate/free
//       the memory of dynamic object of MMap, because the
//       virtual-function-pointers-table is needed.
template <class Tsrc, class Ttgt, class TAB_Ttgt,
          class HF = HashFuncBase<Tsrc> >
class MMap : public HMap<Tsrc, TAB_Ttgt*, HF> {
protected:
    bool m_is_init;
public:
    MMap()
    {
        m_is_init = false;
        init();
    }
    COPY_CONSTRUCTOR(MMap);
    virtual ~MMap() { destroy(); }

    UINT count_mem() const
    {
        return ((HMap<Tsrc, TAB_Ttgt*, HF>*)this)->count_mem() +
               sizeof(m_is_init);
    }

    //Establishing mapping in between 't' and 'mapped'.
    void set(Tsrc t, Ttgt mapped)
    {
        if (t == Tsrc(0)) return;
        TAB_Ttgt * tgttab = HMap<Tsrc, TAB_Ttgt*, HF>::get(t);
        if (tgttab == NULL) {
            //Do NOT use Hash::xmalloc(sizeof(TAB_Ttgt)) to allocate memory,
            //because __vfptr is NULL. __vfptr points to TAB_Ttgt::vftable.
            tgttab = new TAB_Ttgt;
            HMap<Tsrc, TAB_Ttgt*, HF>::set(t, tgttab);
        }
        tgttab->append_node(mapped);
    }

    //Get mapped elements of 't'
    TAB_Ttgt * get(Tsrc t)
    { return HMap<Tsrc, TAB_Ttgt*, HF>::get(t); }

    void init()
    {
        if (m_is_init) { return; }
        HMap<Tsrc, TAB_Ttgt*, HF>::init();
        m_is_init = true;
    }

    void destroy()
    {
        if (!m_is_init) { return; }
        TAB_Ttgt * tgttab = get(HMap<Tsrc, TAB_Ttgt*, HF>::get_first());
        UINT n = HMap<Tsrc, TAB_Ttgt*, HF>::get_elem_count();
        for (UINT i = 0; i < n; i++) {
            ASSERT0(tgttab);
            delete tgttab;
            tgttab = HMap<Tsrc, TAB_Ttgt*, HF>::
                            get(HMap<Tsrc, TAB_Ttgt*, HF>::get_next());
        }

        HMap<Tsrc, TAB_Ttgt*, HF>::destroy();
        m_is_init = false;
    }
};

} //namespace xcom
#endif
