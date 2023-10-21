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
    DUMMYUSE(pool);
    return ::operator new(size);
}


template <class T>
void operator delete(void * ptr, xcom::allocator<T> * pool)
{
    DUMMYUSE(pool);
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
    for (UINT c = 0; list != nullptr; c++, list = list->next) {
        if (list == t) { return c; }
    }
    UNREACHABLE(); //not find.
    return 0;
}


template <class T>
inline UINT cnt_list(T const* t)
{
    UINT c = 0;
    while (t != nullptr) { c++; t = t->next; }
    return c;
}


//Return true if p is in current list.
template <class T>
bool in_list(T const* head, T const* p)
{
    if (p == nullptr) { return true; }
    T const* t = head;
    while (t != nullptr) {
        if (t == p) { return true; }
        t = t->next;
    }
    return false;
}


template <class T>
inline T * get_head(T * t)
{
    while (t != nullptr && t->prev != nullptr) { t = t->prev; }
    return t;
}


template <class T>
inline T * get_last(T * t)
{
    while (t != nullptr && t->next != nullptr) { t = t->next; }
    return t;
}


template <class T>
inline T * get_first(T * t)
{
    while (t != nullptr && t->prev != nullptr) { t = t->prev; }
    return t;
}


//t: may be single element or list.
template <class T>
inline void add_next(T ** pheader, T * t)
{
    ASSERT0(pheader);
    if (t == nullptr) { return; }
    t->prev = nullptr;
    if ((*pheader) == nullptr) {
        *pheader = t;
        return;
    }
    T * p = *pheader;
    ASSERTN(p != t, ("\n<add_next> : overlap list member\n"));
    while (p->next != nullptr) {
        p = p->next;
        ASSERTN(p != t, ("\n<add_next> : overlap list member\n"));
    }
    p->next = t;
    t->prev = p;
}


//t: may be single element or list.
template <class T>
inline void add_next_single_list(MOD T ** pheader, T * t)
{
    if (t == nullptr) { return; }
	if (nullptr == (*pheader)){
	    *pheader = t;
        return;
	}
    T * p = *pheader;
    ASSERTN(p != t, ("\n<add_next_single_list> : overlap list member\n"));
	for (; p->next != nullptr; p = p->next) {
        ASSERTN(p != t, ("\n<add_next_single_list> : overlap list member\n"));
    }
    p->next = t;
}


//t: may be single element or list.
template <class T>
inline void add_next(MOD T ** pheader, MOD T ** last, IN T * t)
{
    if (t == nullptr) { return; }
    ASSERT0(pheader && last);
    t->prev = nullptr;
    if ((*pheader) == nullptr) {
        *pheader = t;
        while (t->next != nullptr) { t = t->next; }
        *last = t;
        return;
    }
    ASSERTN(*last && (*last)->next == nullptr, ("must be the last"));
    (*last)->next = t;
    t->prev = *last;
    while (t->next != nullptr) { t = t->next; }
    *last = t;
}


template <class T>
inline void add_next_single_list(MOD T ** pheader, MOD T ** last, T * t)
{
    if (t == nullptr) { return; }
    ASSERT0(pheader && last);
	if (nullptr == (*pheader)){
	    *pheader = t;
        *last = t;
        return;
	}
    ASSERTN(*last && (*last)->next == nullptr, ("must be the last"));
    (*last)->next = t;
    while (t->next != nullptr) { t = t->next; }
    *last = t;
}


template <class T>
inline void remove(T ** pheader, T * t)
{
    if (pheader == nullptr || t == nullptr) { return; }
    if (t == *pheader) {
        *pheader = t->next;
        if (*pheader != nullptr) {
            (*pheader)->prev = nullptr;
        }
        t->next = t->prev = nullptr;
        return;
    }

    ASSERTN(t->prev, ("t is not in list"));
    t->prev->next = t->next;
    if (t->next != nullptr) {
        t->next->prev = t->prev;
    }
    t->next = t->prev = nullptr;
}


//Swap value of t1 and t2.
//Note the copy constructor may be invoked if T is object type.
template <class T>
inline void swap(T & t1, T & t2)
{
    T t = t1;
    t1 = t2;
    t2 = t;
}


//Swap t1 and t2 in list.
template <class T>
inline void swap(T ** pheader, T * t1, T * t2)
{
    ASSERT0(pheader);
    T * t1p = t1->prev;
    T * t1n = t1->next;
    T * t2p = t2->prev;
    T * t2n = t2->next;

    if (t2 == t1n) {
        t2->next = t1;
    } else {
        t2->next = t1n;
    }
    if (t2 == t1p) {
        t2->prev = t1;
    } else {
        t2->prev = t1p;
    }
    if (t1 == t2n) {
        t1->next = t2;
    } else {
        t1->next = t2n;
    }
    if (t1 == t2p) {
        t1->prev = t2;
    } else {
        t1->prev = t2p;
    }

    if (t1p != nullptr  && t1p != t2) {
        t1p->next = t2;
    }
    if (t1n != nullptr && t1n != t2) {
        t1n->prev = t2;
    }
    if (t2p != nullptr && t2p != t1) {
        t2p->next = t1;
    }
    if (t2n != nullptr && t2n != t1) {
        t2n->prev = t1;
    }
    if (*pheader == t1) {
        *pheader = t2;
    } else if (*pheader == t2) {
        *pheader = t1;
    }
}


template <class T>
inline void replace(T ** pheader, T * olds, T * news)
{
    if (pheader == nullptr || olds == nullptr) { return; }
    if (olds == news) { return; }
    if (news == nullptr) {
        xcom::remove(pheader, olds);
        return;
    }

    #ifdef _DEBUG_
    bool find = false;
    T * p = *pheader;
    while (p != nullptr) {
        if (p == olds) {
            find = true;
            break;
        }
        p = p->next;
    }
    ASSERTN(find, ("'olds' is not inside in pheader"));
    #endif

    news->prev = olds->prev;
    news->next = olds->next;
    if (olds->prev != nullptr) {
        olds->prev->next = news;
    }
    if (olds->next != nullptr) {
        olds->next->prev = news;
    }
    if (olds == *pheader) {
        *pheader = news;
    }
    olds->next = olds->prev = nullptr;
}


template <class T>
inline T * removehead(T ** pheader)
{
    if (pheader == nullptr || *pheader == nullptr) { return nullptr; }
    T * t = *pheader;
    *pheader = t->next;
    if (*pheader != nullptr) {
        (*pheader)->prev = nullptr;
    }
    t->next = t->prev = nullptr;
    return t;
}


template <class T>
inline T * removehead_single_list(T ** pheader)
{
    if (pheader == nullptr || *pheader == nullptr) { return nullptr; }
    T * t = *pheader;
    *pheader = t->next;
    t->next = nullptr;
    return t;
}


template <class T>
inline T * removetail(T ** pheader)
{
    if (pheader == nullptr || *pheader == nullptr) { return nullptr; }
    T * t = *pheader;
    while (t->next != nullptr) { t = t->next; }
    remove(pheader, t);
    return t;
}


//Insert one elem 't' before 'marker'.
template <class T>
inline void insertbefore_one(T ** head, T * marker, T * t)
{
    if (t == nullptr) { return; }
    ASSERTN(head, ("absent parameter"));
    ASSERTN(t != marker, ("t and marker should not be same"));
    if (*head == nullptr) {
        ASSERTN(marker == nullptr, ("marker must be nullptr"));
        *head = t;
        return;
    }
    if (marker == *head) {
        //'marker' is head, replace head.
        t->prev = nullptr;
        t->next = marker;
        marker->prev = t;
        *head = t;
        return;
    }
    ASSERTN(marker->prev != nullptr, ("marker is head"));
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
    if (t == nullptr) { return; }
    ASSERTN(head, ("absent parameter"));
    ASSERTN(t != marker, ("t and marker should not be same"));
    if (*head == nullptr) {
        ASSERTN(marker == nullptr, ("marker must be nullptr"));
        *head = t;
        return;
    }
    ASSERT0(marker);
    if (marker == *head) {
        //'marker' is head, replace head.
        ASSERTN(t->prev == nullptr, ("t should be the first element"));
        add_next(&t, marker);
        *head = t;
        return;
    }
    marker->prev->next = t;
    t->prev = marker->prev;
    t = get_last(t);
    t->next = marker;
    marker->prev = t;
}


//Insert t into list immediately that following 'marker'.
//e.g: a->maker->b->c
//     output is: a->maker->t->b->c
//Return header in 'marker' if list is empty.
template <class T>
inline void insertafter_one(T ** marker, T * t)
{
    if (marker == nullptr || t == nullptr) { return; }
    ASSERTN(t != *marker, ("t and marker should not be same"));
    if (*marker == nullptr) {
        *marker = t;
        return;
    }
    T * last = get_last(t);
    if ((*marker)->next != nullptr) {
        (*marker)->next->prev = last;
        last->next = (*marker)->next;
    }
    (*marker)->next = t;
    t->prev = *marker;
}


//Append t into head of list.
//e.g: given head->a->b->c, and t1->t2.
//     output is: t1->t2->a->b->c
//This function will update the head of list.
template <class T>
inline void append_head(T ** head, T * t)
{
    if (*head == nullptr) {
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
    if (marker == nullptr || t == nullptr) { return; }
    ASSERTN(t != *marker, ("t and marker should not be same"));
    if (*marker == nullptr) {
        *marker = t;
        return;
    }
    if ((*marker)->next != nullptr) {
        T * last = get_last(t);
        (*marker)->next->prev = last;
        last->next = (*marker)->next;
    }
    t->prev = *marker;
    (*marker)->next = t;
}


//The function reverse the order of elements in buf.
//e.g: given buf is <a,b,c,d,e>, after reversing the buf become <e,d,c,b,a>.
template <class T>
inline void reverse_buffer(MOD T * buf, size_t buflen)
{
    for (INT start = 0, end = ((INT)buflen) - 1;
         start <= end - 1; start++, end--) {
        xcom::swap(buf[start], buf[end]);
    }
}


//The function does cyclic shift to elements in buf.
//e.g: given buf is <a,b,c,d,e>, n is 2, after rotation the buf become
//<d,e,a,b,c>.
//n: rotate times.
template <class T>
void rotate_buffer(MOD T * buf, size_t buflen, UINT n)
{
    ASSERT0(buf && n <= buflen);
    xcom::reverse_buffer(buf, buflen);
    xcom::reverse_buffer(buf, n);
    xcom::reverse_buffer(buf + n, buflen - n);
}


//Reverse list, return the new list-head.
template <class T>
inline T * reverse_list(T * t)
{
    T * head = t;
    while (t != nullptr) {
        T * tmp = t->prev;
        t->prev = t->next;
        t->next = tmp;
        head = t;
        t = t->prev;
    }
    return head;
}


//Double Chain Container.
//This class defined the data structure that used to iterate element in List.
#define C_val(c) ((c)->value)
#define C_next(c) ((c)->next)
#define C_prev(c) ((c)->prev)
template <class T> class C {
    COPY_CONSTRUCTOR(C<T>);
public:
    C<T> * prev;
    C<T> * next;
    T value;
public:
    C() { init(); }
    void init()
    {
        prev = next = nullptr;
        value = T(0); //The default value of container.
    }

    T val() { return value; }
    C<T> * get_prev() const { return prev; }
    C<T> * get_next() const { return next; }
};


//Single Chain Container.
//This class defined data structure that used to regard as container for
//elelments in single chain. This data structure always used to iterate
//SListCore, SListCoreEx, SList and SListEx.
#define SC_val(c) ((c)->value)
#define SC_next(c) ((c)->next)
template <class T> class SC {
    COPY_CONSTRUCTOR(SC<T>);
public:
    SC<T> * next;
    T value;
public:
    SC() { init(); }
    void init()
    {
        next = nullptr;
        value = T(0);
    }

    T val() { return value; }
    SC<T> * get_next() const { return next; }
};


//This class defined a single chain list to record free-element, and
//supplies methods that used to operate freed element.
//T refer to basis element type.
//    e.g: Suppose variable type is 'Var*', then T is 'Var'.
//
//For easing implementation, there are 2 fields should be declared in T,
//    struct T {
//        T * next;
//        T * prev;
//        ... //other field
//    }
template <class T>
class FreeList {
    COPY_CONSTRUCTOR(FreeList);
public:
    bool m_is_clean;
    T * m_tail;

public:
    FreeList() { m_is_clean = true; m_tail = nullptr; }
    ~FreeList() { m_tail = nullptr; }

    //Count memory usage for current object.
    size_t count_mem() const { return sizeof(FreeList<T>); }
    //Note the element in list should be freed by user.
    void clean() { m_tail = nullptr; }

    //True if invoke ::memset when user query free element.
    void set_clean(bool is_clean) { m_is_clean = (BYTE)is_clean; }

    //Add t to tail of the list.
    //Do not clean t's content.
    inline void add_free_elem(T * t)
    {
        if (t == nullptr) { return; }
        ASSERT0(t->next == nullptr && t->prev == nullptr); //clean by user.
        if (m_tail == nullptr) {
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
        if (m_tail == nullptr) { return nullptr; }
        T * t = m_tail;
        m_tail = m_tail->prev;
        if (m_tail != nullptr) {
            ASSERT0(t->next == nullptr);
            m_tail->next = nullptr;
            m_is_clean ? ::memset((void*)t, 0, sizeof(T)) : t->prev = nullptr;
        } else {
            ASSERT0(t->prev == nullptr && t->next == nullptr);
            if (m_is_clean) {
                ::memset((void*)t, 0, sizeof(T));
            }
        }
        return t;
    }
};
//END FreeList


//Dual Linked List.
//This class defined a double linked list. It grows dynamically when
//insert new element. The searching speed is slow, O(n). The insertion
//speed is fast if container is given, O(1). Accessing head, tail element
//and querying the number of element are fast, O(1).
//NOTE:
//    The following operations are the key points which you should
//    pay attention to:
//    1. If you REMOVE one element, its container will be collect by FREE-List.
//    So if you need a new container, please check the FREE-List first,
//    accordingly, you should first invoke 'get_free_list' which get free
//    containers out from 'm_free_list'.
//    2. If you want to invoke APPEND, please call 'newc' first to
//    allocate a new container memory space, record your elements in
//    container, then APPEND it at list.
template <class T, class Allocator = allocator<T> > class List {
public:
    typedef C<T>* Iter; //the iter to iterate element in list.
protected:
    COPY_CONSTRUCTOR(List);
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
    ~List() { destroy(); }

    inline void init()
    {
        m_elem_count = 0;
        m_head = m_tail = m_cur = nullptr;
        m_free_list.clean();
        m_free_list.set_clean(false);
    }

    inline void destroy()
    {
        C<T> * ct = m_free_list.m_tail;
        while (ct != nullptr) {
            C<T> * t = ct;
            ct = ct->prev;

            //Do not need to invoke destructor of C<T>.
            operator delete(t, &pool);
        }
        ct = m_head;
        while (ct != nullptr) {
            C<T> * t = ct;
            ct = ct->next;

            //Do not need to invoke destructor of C<T>.
            operator delete(t, &pool);
        }
        m_free_list.clean();
        m_elem_count = 0;
        m_head = m_tail = m_cur = nullptr;
    }

    //Return the end of the list.
    C<T> const* end() const { return nullptr; }

    inline C<T> * newc()
    {
        //allocator<T> p;
        C<T> * c = m_free_list.get_free_elem();
        if (c == nullptr) {
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
        while (c != nullptr) {
            C<T> * next = C_next(c);
            C_prev(c) = C_next(c) = nullptr;
            m_free_list.add_free_elem(c);
            c = next;
        }
        m_elem_count = 0;
        m_head = m_tail = m_cur = nullptr;
    }

    void copy(List<T> const& src)
    {
        clean();
        C<T> * it;
        T t = src.get_head(&it);
        for (INT n = src.get_elem_count(); n > 0; n--) {
            append_tail(t);
            t = src.get_next(&it);
        }
    }

    //Count memory usage for current object.
    size_t count_mem() const
    {
        size_t count = sizeof(m_elem_count);
        count += sizeof(m_head);
        count += sizeof(m_tail);
        count += sizeof(m_cur);
        count += m_free_list.count_mem();

        C<T> * ct = m_free_list.m_tail;
        while (ct != nullptr) {
            count += sizeof(C<T>);
            ct = ct->next;
        }

        ct = m_head;
        while (ct != nullptr) {
            count += sizeof(C<T>);
            ct = ct->next;
        }
        return count;
    }

    //Return the list node container of 't'.
    inline C<T> * append_tail(T t)
    {
        C<T> * c = newc();
        ASSERTN(c != nullptr, ("newc return nullptr"));
        C_val(c) = t;
        append_tail(c);
        return c;
    }

    inline void append_tail(C<T> * c)
    {
        if (m_head == nullptr) {
            ASSERTN(m_tail == nullptr, ("tail should be nullptr"));
            m_head = m_tail = c;
            ASSERT0(C_next(m_head) == nullptr && C_prev(m_head) == nullptr);
            m_elem_count = 1;
            return;
        }
        C_prev(c) = m_tail;
        C_next(m_tail) = c;
        m_tail = c;
        ASSERT0(C_next(c) == nullptr);
        m_elem_count++;
        return;
    }

    //This function copy elements in 'src' and
    //append to tail of current list.
    //'src' is unchanged.
    void append_and_copy_to_tail(List<T> const& src)
    {
        C<T> * t = src.m_head;
        if (t == nullptr) { return; }

        if (m_head == nullptr) {
            C<T> * c  = newc();
            ASSERT0(c);
            C_val(c) = C_val(t);

            ASSERTN(m_tail == nullptr, ("tail should be nullptr"));
            m_head = m_tail = c;
            ASSERT0(C_next(c) == nullptr && C_prev(c) == nullptr);
            t = C_next(t);
        }

        for (; t != nullptr; t = C_next(t)) {
            C<T> * c  = newc();
            ASSERT0(c);
            C_val(c) = C_val(t);
            C_prev(c) = m_tail;
            C_next(m_tail) = c;
            m_tail = c;
        }
        C_next(m_tail) = nullptr;
        m_elem_count += src.get_elem_count();
    }

    //Append value t to head of list.
    //Return the list node container of 't'.
    inline C<T> * append_head(T t)
    {
        C<T> * c  = newc();
        ASSERTN(c, ("newc return nullptr"));
        C_val(c) = t;
        append_head(c);
        return c;
    }

    //Append container to head of list.
    inline void append_head(C<T> * c)
    {
        if (m_head == nullptr) {
            ASSERTN(m_tail == nullptr, ("tail should be nullptr"));
            m_head = m_tail = c;
            ASSERT0(C_next(m_head) == nullptr && C_prev(m_head) == nullptr);
            m_elem_count = 1;
            return;
        }
        C_next(c) = m_head;
        C_prev(m_head) = c;
        m_head = c;
        ASSERT0(C_prev(c) == nullptr);
        m_elem_count++;
        return;
    }

    //This function copy all elements in 'src' and
    //append to current list head.
    void append_and_copy_to_head(List<T> const& src)
    {
        C<T> * t = src.m_tail;
        if (t == nullptr) { return; }

        if (m_head == nullptr) {
            C<T> * c  = newc();
            ASSERT0(c);
            C_val(c) = C_val(t);
            ASSERTN(m_tail == nullptr, ("tail should be nullptr"));
            m_head = m_tail = c;
            ASSERT0(C_next(c) == nullptr && C_prev(c) == nullptr);
            t = C_prev(t);
        }

        for (; t != nullptr; t = C_prev(t)) {
            C<T> * c  = newc();
            ASSERT0(c);
            C_val(c) = C_val(t);
            C_next(c) = m_head;
            C_prev(m_head) = c;
            m_head = c;
        }
        C_prev(m_head) = nullptr;
        m_elem_count += src.get_elem_count();
    }

    //This function will remove all elements in 'src' and
    //append to current list head.
    //Note 'src' will be clean.
    inline void move_head(MOD List<T> & src)
    {
        if (m_tail == nullptr) {
            m_head = src.m_head;
            m_tail = src.m_tail;
            m_elem_count = src.m_elem_count;
            src.m_head = nullptr;
            src.m_tail = nullptr;
            src.m_elem_count = 0;
            return;
        }

        if (src.m_head == nullptr) { return; }
        C_prev(m_head) = src.m_tail;
        C_next(src.m_tail) = m_head;
        m_elem_count += src.m_elem_count;
        m_head = src.m_head;
        src.m_head = nullptr;
        src.m_tail = nullptr;
        src.m_elem_count = 0;
    }

    //This function will remove all elements in 'src' and
    //append to tail of current list.
    //Note 'src' will be clean.
    inline void move_tail(MOD List<T> & src)
    {
        if (m_tail == nullptr) {
            m_head = src.m_head;
            m_tail = src.m_tail;
            m_elem_count = src.m_elem_count;
            src.m_head = nullptr;
            src.m_tail = nullptr;
            src.m_elem_count = 0;
            return;
        }

        if (src.m_head == nullptr) { return; }
        C_next(m_tail) = src.m_head;
        C_prev(src.m_head) = m_tail;
        m_elem_count += src.m_elem_count;

        ASSERT0(src.m_tail != nullptr);
        m_tail = src.m_tail;

        src.m_head = nullptr;
        src.m_tail = nullptr;
        src.m_elem_count = 0;
    }

    //Return true if p is in current list.
    bool in_list(C<T> const* p) const
    {
        if (p == nullptr) { return true; }
        C<T> const* t = m_head;
        while (t != nullptr) {
            if (t == p) { return true; }
            t = C_next(t);
        }
        return false;
    }

    //Insert value t before marker.
    //Note this function will do searching for t and marker, so it is
    //a costly function, and used it be carefully.
    //Return the list node container of 't'.
    C<T> * insert_before(T t, T marker)
    {
        ASSERTN(t != marker, ("element of list cannot be identical"));
        if (m_head == nullptr || marker == C_val(m_head)) {
            return append_head(t);
        }

        C<T> * c = newc();
        ASSERT0(c);
        C_val(c) = t;

        ASSERT0(m_tail);
        if (marker == m_tail->val()) {
            if (C_prev(m_tail) != nullptr) {
                C_next(C_prev(m_tail)) = c;
                C_prev(c) = C_prev(m_tail);
            }
            C_next(c) = m_tail;
            C_prev(m_tail) = c;
        } else {
            //find marker
            C<T> * mc = m_head;
            while (mc != nullptr) {
                if (mc->val() == marker) {
                    break;
                }
                mc = C_next(mc);
            }
            if (mc == nullptr) { return nullptr; }

            if (C_prev(mc) != nullptr) {
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
        ASSERT0(marker && c && C_prev(c) == nullptr && C_next(c) == nullptr);
        ASSERT0(c != marker);
        ASSERT0(m_tail);

        #ifdef _SLOW_CHECK_
        ASSERT0(in_list(marker));
        #endif

        if (C_prev(marker) != nullptr) {
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
    //Return the list node container of 't'.
    inline C<T> * insert_before(T t, IN C<T> * marker)
    {
        C<T> * c = newc();
        ASSERTN(c, ("newc return nullptr"));
        C_val(c) = t;
        insert_before(c, marker);
        return c;
    }

    //Insert 'src' before 'marker', and return the CONTAINER
    //of src head and src tail.
    //This function move all element in 'src' into current list.
    void insert_before(MOD List<T> & src, IN C<T> * marker,
                       OUT C<T> ** list_head_ct = nullptr,
                       OUT C<T> ** list_tail_ct = nullptr)
    {
        if (src.m_head == nullptr) { return; }
        ASSERT0(m_head && marker);
        #ifdef _SLOW_CHECK_
        ASSERT0(in_list(marker));
        #endif
        ASSERT0(src.m_tail);

        if (C_prev(marker) != nullptr) {
            C_next(C_prev(marker)) = src.m_head;
            C_prev(src.m_head) = C_prev(marker);
        }

        C_next(src.m_tail) = marker;
        C_prev(marker) = src.m_tail;
        m_elem_count += src.m_elem_count;

        if (marker == m_head) {
            m_head = src.m_head;
        }

        if (list_head_ct != nullptr) {
            *list_head_ct = src.m_head;
        }

        if (list_tail_ct != nullptr) {
            *list_tail_ct = src.m_tail;
        }

        src.m_head = nullptr;
        src.m_tail = nullptr;
        src.m_elem_count = 0;
    }

    //Insert 'list' before 'marker', and return the CONTAINER
    //of list head and list tail.
    inline void insert_and_copy_before(List<T> const& list, T marker,
                                       OUT C<T> ** list_head_ct = nullptr,
                                       OUT C<T> ** list_tail_ct = nullptr)
    {
        C<T> * ct = nullptr;
        find(marker, &ct);
        insert_before(list, ct, list_head_ct, list_tail_ct);
    }

    //Insert 'list' before 'marker', and return the CONTAINER
    //of list head and list tail.
    void insert_and_copy_before(List<T> const& list, IN C<T> * marker,
                                OUT C<T> ** list_head_ct = nullptr,
                                OUT C<T> ** list_tail_ct = nullptr)
    {
        if (list.m_head == nullptr) { return; }
        ASSERT0(marker);
        C<T> * list_ct = list.m_tail;
        marker = insert_before(list_ct->val(), marker);
        if (list_tail_ct != nullptr) {
            *list_tail_ct = marker;
        }
        list_ct = C_prev(list_ct);
        C<T> * prev_ct = marker;

        for (; list_ct != nullptr; list_ct = C_prev(list_ct)) {
            marker = insert_before(list_ct->val(), marker);
            prev_ct = marker;
        }

        if (list_head_ct != nullptr) {
            *list_head_ct = prev_ct;
        }
    }

    //Insert value t after marker.
    //Note this function will do searching for t and marker, so it is
    //a costly function, and used it be carefully.
    //Return the list node container of 't'.
    C<T> * insert_after(T t, T marker)
    {
        ASSERTN(t != marker,("element of list cannot be identical"));
        if (m_tail == nullptr || marker == m_tail->val()) {
            append_tail(t);
            return m_tail;
        }
        ASSERTN(m_head != nullptr, ("Tail is non empty, but head is nullptr!"));
        C<T> * c = newc();
        ASSERTN(c != nullptr, ("newc return nullptr"));
        C_val(c) = t;
        if (marker == m_head->val()) {
            if (C_next(m_head) != nullptr) {
                C_prev(C_next(m_head)) = c;
                C_next(c) = C_next(m_head);
            }
            C_prev(c) = m_head;
            C_next(m_head) = c;
        } else {
            //find marker
            C<T> * mc = m_head;
            while (mc != nullptr) {
                if (mc->val() == marker) {
                    break;
                }
                mc = C_next(mc);
            }
            if (mc == nullptr) return nullptr;
            if (C_next(mc) != nullptr) {
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
        ASSERT0(marker && c && C_prev(c) == nullptr && C_next(c) == nullptr);
        if (c == marker) { return; }

        ASSERT0(m_head);
        #ifdef _SLOW_CHECK_
        ASSERT0(in_list(marker));
        #endif

        if (C_next(marker) != nullptr) {
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
    //Return the list node container of 't'.
    inline C<T> * insert_after(T t, IN C<T> * marker)
    {
        C<T> * c = newc();
        ASSERTN(c != nullptr, ("newc return nullptr"));
        C_val(c) = t;
        insert_after(c, marker);
        return c;
    }

    //Insert 'src' after 'marker', and return the CONTAINER
    //of src head and src tail.
    //This function move all element in 'src' into current list.
    void insert_after(MOD List<T> & src, IN C<T> * marker,
                      OUT C<T> ** list_head_ct = nullptr,
                      OUT C<T> ** list_tail_ct = nullptr)
    {
        if (src.m_head == nullptr) { return; }
        ASSERT0(m_head && marker);
        #ifdef _SLOW_CHECK_
        ASSERT0(in_list(marker));
        #endif
        ASSERT0(src.m_tail);

        if (C_next(marker) != nullptr) {
            C_prev(C_next(marker)) = src.m_tail;
            C_next(src.m_tail) = C_next(marker);
        }

        C_prev(src.m_head) = marker;
        C_next(marker) = src.m_head;
        m_elem_count += src.m_elem_count;

        if (marker == m_tail) {
            m_tail = src.m_tail;
        }

        if (list_head_ct != nullptr) {
            *list_head_ct = src.m_head;
        }

        if (list_tail_ct != nullptr) {
            *list_tail_ct = src.m_tail;
        }

        src.m_head = nullptr;
        src.m_tail = nullptr;
        src.m_elem_count = 0;
    }

    //Insert 'list' after 'marker', and return the CONTAINER
    //of list head and list tail.
    inline void insert_and_copy_after(List<T> const& list, T marker,
                                      OUT C<T> ** list_head_ct = nullptr,
                                      OUT C<T> ** list_tail_ct = nullptr)
    {
        C<T> * ct = nullptr;
        find(marker, &ct);
        insert_after(list, ct, list_head_ct, list_tail_ct);
    }

    //Insert 'list' after 'marker', and return the CONTAINER
    //of head and tail of members in 'list'.
    void insert_and_copy_after(List<T> const& list, IN C<T> * marker,
                               OUT C<T> ** list_head_ct = nullptr,
                               OUT C<T> ** list_tail_ct = nullptr)
    {
        if (list.m_head == nullptr) { return; }

        ASSERT0(marker);
        C<T> * list_ct = list.m_head;
        marker = insert_after(list_ct->val(), marker);

        if (list_head_ct != nullptr) {
            *list_head_ct = marker;
        }

        list_ct = C_next(list_ct);
        C<T> * prev_ct = marker;

        for (; list_ct != nullptr; list_ct = C_next(list_ct)) {
            marker = insert_after(list_ct->val(), marker);
            prev_ct = marker;
        }

        if (list_tail_ct != nullptr) {
            *list_tail_ct = prev_ct;
        }
    }

    UINT get_elem_count() const { return m_elem_count; }

    inline T get_cur() const
    {
        if (m_cur == nullptr) { return T(0); }
        return m_cur->val();
    }

    //Return m_cur and related container, and it does not modify m_cur.
    inline T get_cur(MOD C<T> ** holder) const
    {
        if (m_cur == nullptr) {
            *holder = nullptr;
            return T(0);
        }
        ASSERT0(holder != nullptr);
        *holder = m_cur;
        return m_cur->val();
    }

    //Get tail of list, return the CONTAINER.
    //This function does not modify m_cur.
    inline T get_tail(OUT C<T> ** holder) const
    {
        ASSERT0(holder);
        *holder = m_tail;
        if (m_tail != nullptr) {
            return m_tail->val();
        }
        return T(0);
    }

    //Get tail of list, return the CONTAINER.
    //This function will modify m_cur.
    inline T get_tail()
    {
        m_cur = m_tail;
        if (m_tail != nullptr) {
            return m_tail->val();
        }
        return T(0);
    }

    //Get head of list, return the CONTAINER.
    //This function will modify m_cur.
    inline T get_head()
    {
        m_cur = m_head;
        if (m_head != nullptr) {
            return m_head->val();
        }
        return T(0);
    }

    //Get head of list, return the CONTAINER.
    //This function does not modify m_cur.
    inline T get_head(OUT C<T> ** holder) const
    {
        ASSERT0(holder);
        *holder = m_head;
        if (m_head != nullptr) {
            return m_head->val();
        }
        return T(0);
    }

    //Get element next to m_cur.
    //This function will modify m_cur.
    inline T get_next()
    {
        if (m_cur == nullptr || m_cur->next == nullptr) {
            m_cur = nullptr;
            return T(0);
        }
        m_cur = m_cur->next;
        return m_cur->val();
    }

    //Return next list node container of 'holder'.
    //Caller could get the element via C_val or val().
    //This function does not modify m_cur.
    inline C<T> * get_next(IN C<T> * holder) const
    {
        ASSERT0(holder);
        return C_next(holder);
    }

    //Return list member and update holder to next member.
    //This function does not modify m_cur.
    inline T get_next(MOD C<T> ** holder) const
    {
        ASSERT0(holder && *holder);
        *holder = C_next(*holder);
        if (*holder != nullptr) {
            return C_val(*holder);
        }
        return T(0);
    }

    //Get element previous to m_cur.
    //This function will modify m_cur.
    inline T get_prev()
    {
        if (m_cur == nullptr || m_cur->prev == nullptr) {
            m_cur = nullptr;
            return T(0);
        }
        m_cur = m_cur->prev;
        return m_cur->val();
    }

    //Return list member and update holder to prev member.
    //This function does not modify m_cur.
    inline T get_prev(MOD C<T> ** holder) const
    {
        ASSERT0(holder && *holder);
        *holder = C_prev(*holder);
        if (*holder != nullptr) {
            return C_val(*holder);
        }
        return T(0);
    }

    //Return prev list node container of 'holder'.
    //Caller could get the element via C_val or val().
    //This function does not modify m_cur.
    inline C<T> * get_prev(IN C<T> * holder) const
    {
        ASSERT0(holder);
        return C_prev(holder);
    }

    //Get element for nth at tail.
    //'n': starting at 0.
    //This function will modify m_cur.
    T get_tail_nth(UINT n, MOD C<T> ** holder = nullptr)
    {
        ASSERTN(n < m_elem_count,("Access beyond list"));
        m_cur = nullptr;
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

        if (holder != nullptr) {
            *holder = c;
        }
        return c->val();
    }

    //Get element for nth at head.
    //'n': getting start with zero.
    //This function will modify m_cur.
    T get_head_nth(UINT n, MOD C<T> ** holder = nullptr)
    {
        ASSERTN(n < m_elem_count,("Access beyond list"));
        m_cur = nullptr;
        if (m_head == nullptr) {
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
        if (holder != nullptr) {
            *holder = c;
        }
        return c->val();
    }

    bool find(IN T t, OUT C<T> ** holder = nullptr) const
    {
        C<T> * tholder;
        if (holder == nullptr) {
            holder = &tholder;
        }
        if (m_head == nullptr) {
            *holder = nullptr;
            return false;
        }
        C<T> * b = m_head;
        C<T> * e = m_tail;
        while (b != e && b != C_next(e)) {
            if (b->val() == t) {
                *holder = b;
                return true;
            }
            if (e->val() == t) {
                *holder = e;
                return true;
            }
            b = C_next(b);
            e = C_prev(e);
        }
        if (b->val() == t) {
            *holder = b;
            return true;
        }
        *holder = nullptr;
        return false;
    }

    void reinit() { destroy(); init(); }

    //Reverse list.
    void reverse()
    {
        C<T> * next_ct;
        for (C<T> * ct = m_head; ct != nullptr; ct = next_ct) {
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

        ASSERTN(m_head, ("list is empty"));

        if (m_head == holder) {
            return remove_head();
        }

        if (m_tail == holder) {
            return remove_tail();
        }

        ASSERTN(C_prev(holder) && C_next(holder), ("illegal t in list"));

        C_next(C_prev(holder)) = C_next(holder);
        C_prev(C_next(holder)) = C_prev(holder);
        m_elem_count--;
        C_prev(holder) = C_next(holder) = nullptr;
        m_free_list.add_free_elem(holder);
        return holder->val();
    }

    //Remove from list, and searching for 't' begin at head.
    T remove(T t)
    {
        if (m_head == nullptr) { return T(0); }
        if (m_head->val() == t) { return remove_head(); }
        if (m_tail->val() == t) { return remove_tail(); }
        C<T> * b = m_head;
        C<T> * e = m_tail;
        while (b != e && b != C_next(e)) {
            if (b->val() == t) {
                return remove(b);
            }
            if (e->val() == t) {
                return remove(e);
            }
            b = C_next(b);
            e = C_prev(e);
        }
        if (b->val() == t) { return remove(b); }
        return T(0);
    }

    //Remove from tail.
    T remove_tail()
    {
        if (m_tail == nullptr) { return T(0); }

        C<T> * c = nullptr;
        if (C_prev(m_tail) == nullptr) {
            //tail is the only one
            ASSERTN(m_tail == m_head && m_elem_count == 1,
                    ("illegal list-remove"));
            c = m_tail;
            m_head = m_tail = m_cur = nullptr;
        } else {
            c = m_tail;
            if (m_cur == m_tail) {
                m_cur = C_prev(m_tail);
            }
            m_tail = C_prev(m_tail);
            C_next(m_tail) = nullptr;
            C_prev(c) = nullptr;
        }

        m_elem_count--;
        m_free_list.add_free_elem(c);
        return c->val();
    }

    //Remove from head.
    T remove_head()
    {
        C<T> * c = nullptr;
        if (m_head == nullptr) { return T(0); }
        if (C_next(m_head) == nullptr) {
            //list_head is the only one
            ASSERTN(m_tail == m_head && m_elem_count == 1,
                    ("illegal list-remove"));
            c = m_head;
            m_head = m_tail = m_cur = nullptr;
        } else {
            c = m_head;
            if (m_cur == m_head) {
                m_cur = C_next(m_head);
            }
            m_head = C_next(m_head);
            C_prev(m_head) = nullptr;
            C_prev(c) = C_next(c) = nullptr;
        }

        m_free_list.add_free_elem(c);
        m_elem_count--;
        return c->val();
    }

    //Set list head container.
    //This function does not modify m_cur.
    void set_head(C<T> * t) { m_head = t; }

    //Set list tail container.
    //This function does not modify m_cur.
    void set_tail(C<T> * t) { m_tail = t; }
};


//Single Linked List Core.
//This class defined the single list with least members. And all memory have
//to allocated by caller.
//This class encapsulates most operations for a single list.
//Note:
//  1. You must invoke init() if the SListCore allocated in mempool.
//  2. The single linked list is different with dual linked list.
//     the dual linked list does not use mempool to hold the container.
//     Compared to dual linked list, single linked list allocate containers
//     in a const size pool.
//  3. The single linked list is not responsible for allocating the memory of
//     container.
//  4. Before going to the destructor, even if the containers have
//     been allocated in memory pool, you should invoke clean() to free
//     all of them back to a free list to reuse them.
template <class T> class SListCoreEx; //forward declaration
template <class T> class SListCore {
    friend class SListCoreEx<T>;
    COPY_CONSTRUCTOR(SListCore);
protected:
    UINT m_elem_count; //list elements counter.
    SC<T> m_head; //list head.
protected:
    SC<T> * new_sc_container(SMemPool * pool)
    {
        ASSERTN(pool, ("need mem pool"));
        ASSERTN(MEMPOOL_type(pool) == MEM_CONST_SIZE, ("need const size pool"));
        SC<T> * p = (SC<T>*)smpoolMallocConstSize(sizeof(SC<T>), pool);
        ASSERT0(p != nullptr);
        ::memset((void*)p, 0, sizeof(SC<T>));
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
        if (free_list == nullptr || *free_list == nullptr) { return nullptr; }
        SC<T> * t = *free_list;
        *free_list = (*free_list)->next;
        t->next = nullptr;
        return t;
    }

    //Find the last element, and return the CONTAINER.
    //This is a costly operation. Use it carefully.
    inline SC<T> * get_tail() const
    {
        SC<T> * c = m_head.next;
        SC<T> * tail = nullptr;
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
        if (c == nullptr) {
            c = new_sc_container(pool);
        }
        return c;
    }
public:
    SListCore() { init(); }
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
        ASSERTN(c != nullptr, ("newsc return nullptr"));
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
        if (tail != nullptr) {
            tail->next = *free_list;
            *free_list = m_head.next;
            m_head.next = &m_head;
            m_elem_count = 0;
        }

        ASSERT0(m_elem_count == 0);
    }

    //Count memory usage for current object.
    size_t count_mem() const
    {
        size_t count = sizeof(m_elem_count);
        count += sizeof(m_head);
        //Do not count SC, they belong to outside pool.
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
    inline SC<T> * insert_after(T t, IN SC<T> * marker, SC<T> ** free_list,
                                SMemPool * pool)
    {
        ASSERT0(marker);
        #ifdef _SLOW_CHECK_
        ASSERT0(in_list(marker));
        #endif

        SC<T> * c = newsc(free_list, pool);
        ASSERTN(c != nullptr, ("newc return nullptr"));

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
    bool find(IN T t, OUT SC<T> ** holder = nullptr) const
    {
        SC<T> * c = m_head.next;
        while (c != &m_head) {
            if (c->val() == t) {
                if (holder != nullptr) {
                    *holder = c;
                }
                return true;
            }
            c = c->next;
        }

        if (holder != nullptr) {
            *holder = nullptr;
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
        ASSERTN(m_head.next != &m_head, ("list is empty"));
        ASSERT0(prev);

        ASSERTN(prev->next == holder, ("not prev one"));
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

    //Reverse elements in list.
    //e.g: List is a->b->c->d, after rervse,
    //list will be d->c->b->a.
    void reverse_list()
    {
        SC<T> * head = nullptr;
        SC<T> * cur = m_head.next;
        while (cur != &m_head) {
            SC<T> * temp = cur->next;
            if (head == nullptr) {
                head = cur;
                head->next = &m_head;
            } else {
                cur->next = head;
                head = cur;
            }
            cur = temp;
        }
        m_head.next = head;
    }
};
//END SListCore


//Exteneded Single Linked List Core.
//This class extends the data structure of SListCore through
//add a tail pointer. That allows you accessing tail element directly
//via get_tail(). This will be useful if you
//are going to append element at the tail of single linked list.
//
//Note:
//  1. You must invoke init() at constrution.
//  2. The single linked list is not responsible for allocating the memory of
//     container.
//  3. Before going to the destructor, even if the containers have
//     been allocated in memory pool, you should invoke clean() to free
//     all of them back to a free list to reuse them.
template <class T> class SListCoreEx {
    COPY_CONSTRUCTOR(SListCoreEx);
protected:
    SListCore<T> m_slcore;
    SC<T> * m_tail; //list tail.
public:
    SListCoreEx() { init(); }
    ~SListCoreEx()
    {
        //Note: Before invoked the destructor, even if the containers have
        //been allocated in memory pool, you should invoke clean() to
        //free all of them back to a free list to reuse them.
    }

    void append_head(SC<T> * c)
    {
        m_slcore.append_head(c);
        if (m_tail == m_slcore.end()) {
            m_tail = c;
        }
    }

    //Find the last element, and add 'c' after it.
    //This is a costly operation. Use it carefully.
    void append_tail(SC<T> * c)
    {
        m_slcore.insert_after(c, m_tail);
        m_tail = c;
    }

    //Count memory usage for current object.
    size_t count_mem() const
    {
        size_t count = sizeof(*this);
        count -= sizeof(SListCore<T>);
        count += m_slcore.count_mem();
        //Do not count SC, they belong to outside pool.
        return count;
    }

    //Return the end of the list.
    SC<T> const * end() const { return m_slcore.end(); }

    //Get head of list, return the CONTAINER.
    //You could iterate the list via comparing the container with end().
    SC<T> * get_head() const { return m_slcore.m_head.next; }

    //Return the next container.
    SC<T> * get_next(IN SC<T> * holder) const
    {
        ASSERT0(holder);
        return SC_next(holder);
    }

    void init()
    {
        m_slcore.init();
        m_tail = &m_slcore.m_head;
    }

    //Return the element removed.
    T remove_head(SC<T> ** free_list)
    { return m_slcore.remove_head(free_list); }

    //Return the element removed.
    //'prev': the previous one element of 'holder'.
    T remove(SC<T> * prev, SC<T> * holder, SC<T> ** free_list)
    { return m_slcore.remove(prev, holder, free_list); }
};
//END SListCoreEx


//Single Linked List
//This class defined general single linked list operations.
//For convenient purpose, this class uses a free list to recycle containers
//when you remove element from list.
//It grows dynamically when insert new element.
//The searching speed is slow, O(n). The insertion and removing speed are
//fast if container is given, O(1).
//Accessing head, querying the number of element are fast, O(1).
//Accessing tail element and remove element without container are slow, O(n).
//
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
//    5. Compare the iterator with end() to determine if meeting the end
//       of list.
//    6. Byte size of element in Const Pool is equal to sizeof(SC<T>).
//
//    Usage:SMemPool * pool = smpoolCreate(sizeof(SC<T>) * n, MEM_CONST_SIZE);
//          SList<T> list(pool);
//          SList<T> * list2 = smpoolMalloc();
//          list2->init(pool);
//          ...
//          smpoolDelete(pool);
template <class T> class SList : public SListCore<T> {
    COPY_CONSTRUCTOR(SList);
protected:
    SMemPool * m_free_list_pool;
    SC<T> * m_free_list; //Hold for available containers

public:
    SList(SMemPool * pool = nullptr)
    {
        //Invoke init() explicitly if SList is allocated from mempool.
        set_pool(pool);
    }
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
        ASSERTN(pool == nullptr || MEMPOOL_type(pool) == MEM_CONST_SIZE,
                ("need const size pool"));
        m_free_list_pool = pool;
        m_free_list = nullptr;
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
    //Count memory usage for current object.
    size_t count_mem() const
    {
        //Do not count SC containers, they belong to input pool.
        return SListCore<T>::count_mem() + sizeof(m_free_list_pool) +
               sizeof(m_free_list);
    }

    //Insert 't' into list after the 'marker'.
    SC<T> * insert_after(T t, IN SC<T> * marker)
    {
        ASSERT0(m_free_list_pool);
        return SListCore<T>::insert_after(t, marker, &m_free_list,
                                          m_free_list_pool);
    }

    //Remove 't' out of list, return true if find t, otherwise return false.
    //Note that this is costly operation.
    bool remove(T t)
    {
        ASSERT0(m_free_list_pool);
        return SListCore<T>::remove(t, &m_free_list);
    }

    //Remove elemlent that contained in 'holder' from current single
    //linked list.
    //Return element removed.
    //'prev': the holder of previous element of 'holder'.
    //Note both holders must belong to current SList.
    T remove(SC<T> * prev, SC<T> * holder)
    {
        ASSERT0(m_free_list_pool);
        if (prev == nullptr) {
            T t = remove_head();
            ASSERTN(t == holder->val(),
                    ("prev does not have relation to holder"));
            return t;
        }
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


//The Extended Single Linked List.
//
//This kind of single linked list has a tail pointer that allows you access
//tail element directly via get_tail(). This will be useful if you
//are going to append element at the tail of single linked list.
//
//Encapsulate most operations for single linked list.
//
//Note the single linked list is different with dual linked list.
//the dual linked list does not use mempool to hold the container.
//Compared to dual linked list, single linked list allocate containers
//in a const size pool.
template <class T> class SListEx {
    COPY_CONSTRUCTOR(SListEx);
protected:
    UINT m_elem_count;
    SC<T> * m_head;
    SC<T> * m_tail;
protected:
    SC<T> * new_sc_container(SMemPool * pool)
    {
        ASSERTN(pool, ("need mem pool"));
        ASSERTN(MEMPOOL_type(pool) == MEM_CONST_SIZE, ("need const size pool"));
        SC<T> * p = (SC<T>*)smpoolMallocConstSize(sizeof(SC<T>), pool);
        ASSERT0(p);
        ::memset((void*)p, 0, sizeof(SC<T>));
        return p;
    }

    SC<T> * get_freed_sc(SC<T> ** free_list)
    {
        if (free_list == nullptr || *free_list == nullptr) { return nullptr; }
        SC<T> * t = *free_list;
        *free_list = (*free_list)->next;
        t->next = nullptr;
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
        if (c == nullptr) {
            c = new_sc_container(pool);
        }
        return c;
    }

    //Check p is the element in list.
    bool in_list(SC<T> const* p) const
    {
        if (p == nullptr) { return true; }
        SC<T> const* t = m_head;
        while (t != nullptr) {
            if (t == p) { return true; }
            t = t->next;
        }
        return false;
    }
public:
    SListEx() { init(); }
    ~SListEx()
    {
        //Note: Before going to the destructor, even if the containers have
        //been allocated in memory pool, you should free all of them
        //back to a free list to reuse them.
    }

    void init()
    {
        m_elem_count = 0;
        m_head = nullptr;
        m_tail = nullptr;
    }

    //Return the end of the list.
    SC<T> const* end() const { return nullptr; }

    SC<T> * append_tail(T t, SC<T> ** free_list, SMemPool * pool)
    {
        SC<T> * c  = newsc(free_list, pool);
        ASSERTN(c != nullptr, ("newsc return nullptr"));
        SC_val(c) = t;
        append_tail(c);
        return c;
    }

    void append_tail(IN SC<T> * c)
    {
        if (m_head == nullptr) {
            ASSERTN(m_tail == nullptr, ("tail should be nullptr"));
            ASSERT0(SC_next(c) == nullptr);
            m_head = m_tail = c;
            m_elem_count++;
            return;
        }

        SC_next(m_tail) = c;
        m_tail = c;
        ASSERT0(SC_next(c) == nullptr);
        m_elem_count++;
        return;
    }

    SC<T> * append_head(T t, SC<T> ** free_list, SMemPool * pool)
    {
        SC<T> * c = newsc(free_list, pool);
        ASSERTN(c != nullptr, ("newsc return nullptr"));
        SC_val(c) = t;
        append_head(c);
        return c;
    }

    void append_head(IN SC<T> * c)
    {
        #ifdef _DEBUG_
        if (m_head == nullptr) {
            ASSERTN(m_tail == nullptr, ("tail should be nullptr"));
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
        if (m_tail != nullptr) {
            m_tail->next = *free_list;
            ASSERT0(m_head);
            *free_list = m_head;
            m_head = m_tail = nullptr;
            m_elem_count = 0;
        }
        ASSERT0(m_head == m_tail && m_head == nullptr && m_elem_count == 0);
    }
    //Count memory usage for current object.
    size_t count_mem() const
    {
        size_t count = sizeof(m_elem_count);
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
    inline SC<T> * insert_after(T t,
                                IN SC<T> * marker,
                                SC<T> ** free_list,
                                SMemPool * pool)
    {
        ASSERT0(marker);
        #ifdef _SLOW_CHECK_
        ASSERT0(in_list(marker));
        #endif

        SC<T> * c = newsc(free_list, pool);
        ASSERTN(c, ("newc return nullptr"));

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
    bool find(IN T t, OUT SC<T> ** holder = nullptr) const
    {
        SC<T> * c = m_head;
        while (c != nullptr) {
            if (c->val() == t) {
                if (holder != nullptr) {
                    *holder = c;
                }
                return true;
            }
            c = c->next;
        }

        if (holder != nullptr) {
            *holder = nullptr;
        }
        return false;
    }

    //Remove 't' out of list, return true if find t, otherwise return false.
    //Note that this is costly operation.
    bool remove(T t, SC<T> ** free_list)
    {
        if (m_head == nullptr) { return false; }
        if (m_head->val() == t) {
            remove_head(free_list);
            return true;
        }

        SC<T> * c = m_head->next;
        SC<T> * prev = m_head;
        while (c != nullptr) {
            if (c->val() == t) { break; }
            prev = c;
            c = c->next;
        }

        if (c == nullptr) { return false; }

        remove(prev, c, free_list);
        return true;
    }

    //Return the element removed.
    //'prev': the previous one element of 'holder'.
    T remove(SC<T> * prev, SC<T> * holder, SC<T> ** free_list)
    {
        ASSERT0(holder);
        ASSERTN(m_head != nullptr, ("list is empty"));
        #ifdef _SLOW_CHECK_
        ASSERT0(in_list(prev));
        ASSERT0(in_list(holder));
        #endif

        if (prev == nullptr) {
            ASSERT0(holder == m_head);
            m_head = m_head->next;
            if (m_head == nullptr) {
                ASSERT0(m_elem_count == 1);
                m_tail = nullptr;
            }
        } else {
            ASSERTN(prev->next == holder, ("not prev one"));
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
        if (m_head == nullptr) { return T(0); }

        SC<T> * c = m_head;
        m_head = m_head->next;
        if (m_head == nullptr) {
            m_tail = nullptr;
        }

        T t = c->val();
        free_sc(c, free_list);
        m_elem_count--;
        return t;
    }

    //Reverse elements in list.
    //e.g: List is a->b->c->d, after rervse,
    //list will be d->c->b->a.
    void reverse_list()
    {
        SC<T> * head = nullptr;
        SC<T> * cur = m_head->next;
        while (cur != m_head) {
            SC<T> * temp = cur->next;
            if (head == nullptr) {
                head = cur;
                head->next = nullptr;
                m_tail = cur;
            } else {
                cur->next = head;
                head = cur;
            }
            cur = temp;
        }
        m_head = head;
    }
};
//END SListEx


//The Extended List
//
//This class is an extention to List, it adds a hash-mapping table upon List
//in order to speed up the process when inserting or removing an element without
//'marker' given.
//
//This class defined a double linked list. It grows dynamically when
//insert new element. The searching speed is O(log(n)). The insertion
//speed is fast if container is given, O(1). Accessing head, tail element
//and querying the number of element are fast, O(1).
//
//NOTE: User must define a mapping class. Usually, it can be TMap or Hash.
template <class T, class MapTypename2Holder> class EList : public List<T> {
    COPY_CONSTRUCTOR(EList);
protected:
    MapTypename2Holder m_typename2holder; //map typename 'T' to its list holder.
public:
    typedef C<T>* Iter; //the iter to iterate element in list.
public:
    EList() {}
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
    //Count memory usage for current object.
    size_t count_mem() const
    {
        size_t count = m_typename2holder.count_mem();
        count += ((List<T>*)this)->count_mem();
        return count;
    }

    //Note the function does NOT check whether if 't' has been in the list.
    //User should utilize method find() to fast check before invoke the
    //function.
    C<T> * append_tail(T t)
    {
        C<T> * c = List<T>::append_tail(t);
        m_typename2holder.setAlways(t, c);
        return c;
    }

    //Note the function does NOT check whether if 't' has been in the list.
    //User should utilize method find() to fast check before invoke the
    //function.
    C<T> * append_head(T t)
    {
        C<T> * c = List<T>::append_head(t);
        m_typename2holder.setAlways(t, c);
        return c;
    }

    //Note the function does NOT check whether if 't' has been in the list.
    //User should utilize method find() to fast check before invoke the
    //function.
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

    //Note the function does NOT check whether if 't' has been in the list.
    //User should utilize method find() to fast check before invoke the
    //function.
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

    bool find(T t, C<T> ** holder = nullptr) const
    {
        C<T> * c = m_typename2holder.get(t);
        if (c == nullptr) {
            return false;
        }
        if (holder != nullptr) {
            *holder = c;
        }
        return true;
    }

    MapTypename2Holder * get_holder_map() const { return &m_typename2holder; }

    T get_cur() const //Do NOT update 'm_cur'
    { return List<T>::get_cur(); }

    T get_cur(MOD C<T> ** holder) const //Do NOT update 'm_cur'
    { return List<T>::get_cur(holder); }

    T get_next() //Update 'm_cur'
    { return List<T>::get_next(); }

    T get_prev() //Update 'm_cur'
    { return List<T>::get_prev(); }

    T get_next(MOD C<T> ** holder) const //Do NOT update 'm_cur'
    { return List<T>::get_next(holder); }

    C<T> * get_next(IN C<T> * holder) const //Do NOT update 'm_cur'
    { return List<T>::get_next(holder); }

    T get_prev(MOD C<T> ** holder) const //Do NOT update 'm_cur'
    { return List<T>::get_prev(holder); }

    C<T> * get_prev(IN C<T> * holder) const //Do NOT update 'm_cur'
    { return List<T>::get_prev(holder); }

    T get_next(T marker) const //not update 'm_cur'
    {
        C<T> * holder;
        find(marker, &holder);
        ASSERT0(holder != nullptr);
        return List<T>::get_next(&holder);
    }

    T get_prev(T marker) const //not update 'm_cur'
    {
        C<T> * holder;
        find(marker, &holder);
        ASSERT0(holder != nullptr);
        return List<T>::get_prev(&holder);
    }

    //NOTICE: 'marker' should have been in the list.
    C<T> * insert_before(T t, T marker)
    {
        C<T> * marker_holder = m_typename2holder.get(marker);
        if (marker_holder == nullptr) {
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
        if (marker_holder == nullptr) {
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

    //Return the container of 'newt'.
    C<T> * replace(T oldt, T newt)
    {
        C<T> * old_holder = m_typename2holder.get(oldt);
        ASSERTN(old_holder != nullptr, ("old elem not exist"));

        //add new one
        C<T> * new_holder = List<T>::insert_before(newt, old_holder);
        m_typename2holder.setAlways(newt, new_holder);

        //remove old one
        m_typename2holder.setAlways(oldt, nullptr);
        List<T>::remove(old_holder);
        return new_holder;
    }

    T remove(T t)
    {
        C<T> * c = m_typename2holder.get(t);
        if (c == nullptr) {
            return T(0);
        }
        T tt = List<T>::remove(c);
        m_typename2holder.setAlways(t, nullptr);
        return tt;
    }

    T remove(C<T> * holder)
    {
        ASSERT0(m_typename2holder.get(holder->val()) == holder);
        T t = List<T>::remove(holder);
        m_typename2holder.setAlways(t, nullptr);
        return t;
    }

    T remove_tail()
    {
        T t = List<T>::remove_tail();
        m_typename2holder.setAlways(t, nullptr);
        return t;
    }

    T remove_head()
    {
        T t = List<T>::remove_head();
        m_typename2holder.setAlways(t, nullptr);
        return t;
    }
};
//END EList


//STACK
template <class T> class Stack : public List<T> {
    COPY_CONSTRUCTOR(Stack);
public:
    Stack() {}

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
//This class represents vector. The size of vector is extended dynamically.
//T: refer to element type.
//NOTE:
//    1. T() or T(0) are treated as the default object when we determine the
//    element existence. Class T should provide a class-constructor
//    that indicates the default choice. Member functions will return an
//    object that constructed by the default constructor when encountering
//    exception.
//    2. The object allocated in heap.
//    3. Zero is reserved and regard it as the default nullptr when we
//    determine whether an element is exist.
typedef INT VecIdx; //VecIdx must be signed integer type.
#define VEC_UNDEF ((VecIdx)-1) //UNDEF value must be -1
#define IS_VECUNDEF(x) (((VecIdx)x) == VEC_UNDEF)

template <class T> class Vector {
protected:
    typedef UINT ElemNumTy;
    bool m_is_init; //To make sure functions are idempotent.
    ElemNumTy m_elem_num; //The number of element in vector.
    VecIdx m_last_idx; //Last element idx
    T * m_vec;
public:
    Vector()
    {
        m_is_init = false;
        init();
    }
    explicit Vector(UINT size)
    {
        m_is_init = false;
        init();
        grow(size);
    }
    Vector(Vector const& vec)
    {
        m_is_init = false;
        init();
        copy(vec);
    }
    Vector const& operator = (Vector const&); //DISALBE COPY-CONSTRUCTOR.
    ~Vector() { destroy(); }

    void append(T t)
    {
        ASSERTN(is_init(), ("VECTOR not yet initialized."));
        set(get_elem_count(), t);
    }

    //Copy each elements of 'list' into vector.
    //NOTE: The default termination factor is '0'.
    //    While we traversing elements of List one by one, or from head to
    //    tail or on opposition way, one can copy list into vector first and
    //    iterating the vector instead of travering list.
    void copy(List<T> & list)
    {
        VecIdx count = 0;
        set((VecIdx)list.get_elem_count() - 1, 0); //Alloc memory right away.
        for (T elem = list.get_head();
             elem != T(0); elem = list.get_next(), count++) {
            set(count, elem);
        }
    }

    void copy(Vector const& vec)
    {
        ASSERT0(vec.m_elem_num > 0 ||
                (vec.m_elem_num == 0 && vec.m_last_idx == VEC_UNDEF));
        ElemNumTy n = vec.m_elem_num;
        if (m_elem_num < n) {
            destroy();
            init(n);
        }
        if (n > 0) {
            ::memcpy(m_vec, vec.m_vec, sizeof(T) * n);
        }
        m_last_idx = vec.m_last_idx;

        //Note the effect of copy is amount to initialization.
        m_is_init = true;
    }

    //Clean to zero(default) till 'last_idx'.
    void clean()
    {
        ASSERTN(is_init(), ("VECTOR not yet initialized."));
        if (m_last_idx == VEC_UNDEF) {
            return;
        }
        ::memset((void*)m_vec, 0, sizeof(T) * (get_elem_count()));
        m_last_idx = VEC_UNDEF; //No any elements
    }

    //Clean element start from 'idx' to the lastidx.
    void cleanFrom(VecIdx idx)
    {
        ASSERTN(is_init(), ("VECTOR not yet initialized."));
        if (m_last_idx == VEC_UNDEF) {
            return;
        }
        ASSERT0(idx < (VecIdx)get_elem_count());
        ::memset((void*)&m_vec[idx], 0, sizeof(T) * (get_elem_count() - idx));
        m_last_idx = idx == 0 ? VEC_UNDEF : idx - 1;
    }

    //Count memory usage for current object.
    size_t count_mem() const
    { return m_elem_num * sizeof(T) + sizeof(Vector<T>); }

    void destroy()
    {
        if (!m_is_init) { return; }
        m_elem_num = 0;
        if (m_vec != nullptr) {
            ::free(m_vec);
        }
        m_vec = nullptr;
        m_last_idx = VEC_UNDEF;
        m_is_init = false;
    }

    //The function often invoked by destructor, to speed up destruction time.
    //Since reset other members is dispensable.
    void destroy_vec()
    {
        if (m_vec != nullptr) {
            ::free(m_vec);
        }
    }

    //The function return the element in vector that indexed by 'index'.
    //Note class-name T should provide a default class-constructor that
    //without any parameters. The function will return an object that
    //constructed by the default constructor if 'index' is out of range.
    T get(VecIdx index) const
    {
        ASSERTN(is_init(), ("VECTOR not yet initialized."));
        return (((ElemNumTy)index) >= m_elem_num) ?
               T(0) : m_vec[(ElemNumTy)index];
    }

    //Return vector buffer that hold elements.
    T * get_vec() { return m_vec; }

    void init()
    {
        if (m_is_init) { return; }
        m_elem_num = 0;
        m_vec = nullptr;
        m_last_idx = VEC_UNDEF;
        m_is_init = true;
    }
    void init(UINT size)
    {
        if (m_is_init) { return; }
        ASSERT0(size != 0);
        m_vec = (T*)::malloc(sizeof(T) * size);
        ASSERT0(m_vec);
        ::memset((void*)m_vec, 0, sizeof(T) * size);
        m_elem_num = (ElemNumTy)size;
        m_last_idx = VEC_UNDEF;
        m_is_init = true;
    }
    bool is_init() const { return m_is_init; }

    //Return true if there is not any element.
    bool is_empty() const { return get_last_idx() == VEC_UNDEF; }

    //Get the address in vector of given element referred by 'idx'.
    T * get_elem_addr(VecIdx idx) const
    {
        ASSERTN(is_init(), ("VECTOR not yet initialized."));
        return m_vec + idx;
    }

    //Return the number of element the vector could hold.
    UINT get_capacity() const
    {
        ASSERTN(is_init(), ("VECTOR not yet initialized."));
        return m_elem_num;
    }

    VecIdx get_last_idx() const
    {
        ASSERTN(is_init(), ("VECTOR not yet initialized."));
        ASSERTN(IS_VECUNDEF(m_last_idx) || m_last_idx < (VecIdx)m_elem_num,
                ("Last index ran over Vector's size."));
        return m_last_idx;
    }

    //Return the number of elements in vector.
    //Note the function computes the number by return the last element
    //index + 1.
    //e.g: the vector has elements <null, x, y, z>, the function return 4.
    UINT get_elem_count() const { return (UINT)(get_last_idx() + 1); }

    //Growing vector up to hold the most num_of_elem elements.
    //If 'm_elem_num' is 0 , alloc vector to hold num_of_elem elements.
    //Reallocate memory if necessory.
    void grow(UINT num_of_elem)
    {
        ASSERTN(is_init(), ("VECTOR not yet initialized."));
        if (num_of_elem == 0) { return; }
        if (m_elem_num == 0) {
            ASSERTN(m_vec == nullptr,
                    ("vector should be nullptr if size is zero."));
            m_vec = (T*)::malloc(sizeof(T) * num_of_elem);
            ASSERT0(m_vec);
            ::memset((void*)m_vec, 0, sizeof(T) * num_of_elem);
            m_elem_num = (ElemNumTy)num_of_elem;
            return;
        }

        ASSERT0((ElemNumTy)num_of_elem > m_elem_num);
        T * tmp = (T*)::malloc(num_of_elem * sizeof(T));
        ASSERT0(tmp);
        ::memcpy((void*)tmp, m_vec, m_elem_num * sizeof(T));
        ::memset((void*)(((CHAR*)tmp) + m_elem_num * sizeof(T)), 0,
                 (num_of_elem - m_elem_num)* sizeof(T));
        ::free(m_vec);
        m_vec = tmp;
        m_elem_num = num_of_elem;
    }

    //Overloaded [] for CONST array reference create an rvalue.
    //Similar to 'get()', the difference between this operation
    //and get() is [] opeartion does not allow index is greater than
    //or equal to m_elem_num.
    //Note this operation can not be used to create lvalue.
    //e.g: Vector<int> const v;
    //    int ex = v[i];
    T const operator[](VecIdx index) const
    {
        ASSERTN(is_init(), ("VECTOR not yet initialized."));
        ASSERTN(((ElemNumTy)index) < m_elem_num,
                ("array subscript over boundary."));
        return m_vec[index];
    }

    //Overloaded [] for non-const array reference create an lvalue.
    //Similar to set(), the difference between this operation
    //and set() is [] opeartion does not allow index is greater than
    //or equal to m_elem_num.
    inline T & operator[](VecIdx index)
    {
        ASSERTN(is_init(), ("VECTOR not yet initialized."));
        ASSERTN(((ElemNumTy)index) < m_elem_num,
                ("array subscript over boundary."));
        m_last_idx = MAX((VecIdx)index, m_last_idx);
        return m_vec[index];
    }

    void reinit() { destroy(); init(); }

    //Place elem to vector according to index.
    //Growing vector if 'index' is greater than m_elem_num.
    void set(VecIdx index, T elem)
    {
        ASSERTN(is_init(), ("VECTOR not yet initialized."));
        if (((ElemNumTy)index) >= m_elem_num) {
            grow(getNearestPowerOf2((UINT)index) + 2);
        }
        m_last_idx = MAX((VecIdx)index, m_last_idx);
        m_vec[index] = elem;
        return;
    }

    //Set vector buffer that will used to hold element.
    //vec: vector buffer pointer
    //     Note if vec is nullptr, that means reset vector buffer to be nullptr.
    //elem_num: the number of element that could store into buffer.
    //          Note the byte size of buffer is equal to elem_num*sizeof(T).
    void set_vec(T * vec, ElemNumTy elem_num)
    {
        ASSERTN(is_init(), ("VECTOR not yet initialized."));
        ASSERT0((vec && elem_num > 0) || (vec == nullptr && elem_num == 0));
        m_vec = vec;
        m_elem_num = elem_num;
    }
};
//END Vector


//The extented class to Vector.
//This class maintains an index which call Free Index of Vector.
//User can use the Free Index to get to know which slot of vector
//is T(0).
//
//e.g: assume Vector<INT> has 6 elements, {0, 3, 4, 0, 5, 0}.
//Three of them are 0, where T(0) served as default nullptr and these
//elements indicate free slot.
//In the case, the first free index is 0, the second is 3, and the
//third is 5.
template <class T, UINT GrowSize = 8>
class VectorWithFreeIndex : public Vector<T> {
    COPY_CONSTRUCTOR(VectorWithFreeIndex);
protected:
    //Always refers to free space to Vector,
    //and the first free space position is '0'
    UINT m_free_idx;

public:
    VectorWithFreeIndex()
    {
        Vector<T>::m_is_init = false;
        init();
    }
    inline void init()
    {
        Vector<T>::init();
        m_free_idx = 0;
    }

    inline void init(UINT size)
    {
        Vector<T>::init(size);
        m_free_idx = 0;
    }

    void copy(VectorWithFreeIndex const& vec)
    {
        Vector<T>::copy(vec);
        m_free_idx = vec.m_free_idx;
    }

    //Clean to zero(default) till 'last_idx'.
    void clean()
    {
        Vector<T>::clean();
        m_free_idx = 0;
    }
    size_t count_mem() const
    { return sizeof(m_free_idx) + Vector<T>::count_mem(); }

    //Return index of free-slot into Vector, and allocate memory
    //if there are not any free-slots.
    //
    //v: default nullptr value.
    //
    //NOTICE:
    //    The condition that we considered a slot is free means the
    //    value of that slot is equal to v.
    UINT get_free_idx(T v = T(0))
    {
        ASSERTN((Vector<T>::is_init()),
                ("VECTOR not yet initialized."));
        if (Vector<T>::m_elem_num == 0) {
            //VECTOR is empty.
            ASSERTN((Vector<T>::m_last_idx == VEC_UNDEF &&
                    Vector<T>::m_vec == nullptr),
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
        for (UINT i = m_free_idx + 1; i < Vector<T>::m_elem_num; i++) {
            if (v == Vector<T>::m_vec[i]) {
                m_free_idx = i;
                return ret;
            }
        }

        //Seaching in first-half Vector to find the next free idx.
        for (UINT i = 0; i < m_free_idx; i++) {
            if (v == Vector<T>::m_vec[i]) {
                m_free_idx = i;
                return ret;
            }
        }

        m_free_idx = Vector<T>::m_elem_num;
        grow(Vector<T>::m_elem_num * 2);
        return ret;
    }

    //Growing vector by size, if 'm_size' is nullptr , alloc vector.
    //Reallocate memory if necessory.
    void grow(UINT size)
    {
        if (Vector<T>::m_elem_num == 0) {
            m_free_idx = 0;
        }
        Vector<T>::grow(size);
    }
};


//Simple Vector.
//This class represents small and lightweith vector. The size of vector is
//extended dynamically.
//GrowSize: the number of element for each grow.
//MaxSize: the maximum number of element that vector can grow up to.
//T: refer to element type.
//NOTE:
//    1. T(0) is treated as the default nullptr when we determine the element
//    existence.
//    2. The object is allocated in mempool, thus it is inefficient to grow
//    vector too many times.
//    3. The destroy() does NOT free any memory, it just reset the status of
//    object to initialization.
#define SVEC_elem_num(s) ((s)->s1.m_elem_num)
template <class T, UINT GrowSize, UINT MaxSize> class SimpleVector {
protected:
    struct {
        UINT m_elem_num:31; //The number of element in vector.
        UINT m_is_init:1; //To make sure functions are idempotent.
    } s1;
public:
    T * m_vec; //vector's memory is allocated in outside mempool.
public:
    SimpleVector()
    {
        s1.m_is_init = false;
        init();
    }
    explicit SimpleVector(UINT size)
    {
        s1.m_is_init = false;
        init(size);
    }
    SimpleVector(SimpleVector const& src, SMemPool * pool) { copy(src, pool); }
    SimpleVector const& operator = (SimpleVector const&);
    ~SimpleVector() { destroy(); }

    //Copy element of src.
    //pool: the vector buffer allocated in the pool.
    void copy(SimpleVector const& src, SMemPool * pool)
    {
        UINT n = SVEC_elem_num(&src);
        UINT tgtn = SVEC_elem_num(this);
        if (tgtn < n) {
            destroy();
            init(n, pool);
        } else if (tgtn > n) {
            ::memset((void*)(((BYTE*)m_vec) + sizeof(T) * n), 0,
                     sizeof(T) * (tgtn - n));
        }
        if (n > 0) {
            ::memcpy(m_vec, src.m_vec, sizeof(T) * n);
        }

        //Note the effect of copy is amount to initialization.
        s1.m_is_init = true;
    }

    //Clean to zero(default) till 'last_idx'.
    void clean()
    {
        ASSERTN(s1.m_is_init, ("SimpleVector not yet initialized."));
        ::memset((void*)m_vec, 0, sizeof(T) * SVEC_elem_num(this));
    }

    //Count memory usage for current object.
    size_t count_mem() const
    { return SVEC_elem_num(this) + sizeof(Vector<T>); }

    inline void init()
    {
        if (s1.m_is_init) { return; }
        SVEC_elem_num(this) = 0;
        m_vec = nullptr;
        s1.m_is_init = true;
    }
    inline void init(UINT size, SMemPool * pool)
    {
        if (s1.m_is_init) { return; }
        ASSERT0(size != 0 && size < MaxSize);
        m_vec = (T*)smpoolMalloc(sizeof(T) * size, pool);
        ASSERT0(m_vec);
        ::memset((void*)m_vec, 0, sizeof(T) * size);
        SVEC_elem_num(this) = size;
        s1.m_is_init = true;
    }
    bool is_init() const { return s1.m_is_init; }

    inline void destroy()
    {
        if (!s1.m_is_init) { return; }
        SVEC_elem_num(this) = 0;
        m_vec = nullptr;
        s1.m_is_init = false;
    }

    T get(UINT i) const
    {
        ASSERTN(s1.m_is_init, ("SimpleVector not yet initialized."));
        if (i >= SVEC_elem_num(this)) { return T(0); }
        return m_vec[i];
    }

    //Return the number of element in the vector.
    UINT getElemNum() const { return SVEC_elem_num(this); }

    //Return the vector buffer that hold elements.
    T * get_vec() { return m_vec; }

    //Return the maximum number of element the vector could hold.
    UINT get_capacity() const
    {
        ASSERTN(is_init(), ("SimpleVector not yet initialized."));
        return SVEC_elem_num(this);
    }

    //Growing vector to 'size' of T, if 'm_vec' is nullptr , alloc vector.
    //Reallocate memory if necessory.
    void grow(UINT size, SMemPool * pool)
    {
        ASSERTN(is_init(), ("SimpleVector not yet initialized."));
        if (size == 0) { return; }
        if (SVEC_elem_num(this) == 0) {
            ASSERTN(m_vec == nullptr,
                    ("SimpleVector should be nullptr if size is zero."));
            ASSERT0(size < MaxSize);
            m_vec = (T*)smpoolMalloc(sizeof(T) * size, pool);
            ASSERT0(m_vec);
            ::memset((void*)m_vec, 0, sizeof(T) * size);
            SVEC_elem_num(this) = size;
            return;
        }

        ASSERT0(size > SVEC_elem_num(this) && size < MaxSize);
        T * tmp = (T*)smpoolMalloc(sizeof(T) * size, pool);
        ASSERT0(tmp);
        ::memcpy(tmp, m_vec, SVEC_elem_num(this) * sizeof(T));
        ::memset((void*)(((CHAR*)tmp) + SVEC_elem_num(this) * sizeof(T)),
                 0, (size - SVEC_elem_num(this))* sizeof(T));
        m_vec = tmp;
        SVEC_elem_num(this) = size;
    }

    //Overloaded [] for CONST array reference create an rvalue.
    //Similar to 'get()', the difference between this operation
    //and get() is [] opeartion does not allow index is greater than
    //or equal to m_elem_num.
    //Note this operation can not be used to create lvalue.
    //e.g: SimpleVector<int> const v;
    //     int ex = v[i];
    T const operator[](UINT index) const
    {
        ASSERTN(is_init(), ("VECTOR not yet initialized."));
        ASSERTN(index < SVEC_elem_num(this),
                ("array subscript over boundary."));
        return m_vec[index];
    }

    //Overloaded [] for non-const array reference create an lvalue.
    //Similar to set(), the difference between this operation
    //and set() is [] opeartion does not allow index is greater than
    //or equal to m_elem_num.
    //e.g: SimpleVector<int> v;
    //     v[i] = 20;
    inline T & operator[](UINT index)
    {
        ASSERTN(is_init(), ("VECTOR not yet initialized."));
        ASSERTN(index < SVEC_elem_num(this),
                ("array subscript over boundary."));
        return m_vec[index];
    }

    void reinit() { destroy(); init(); }

    //Return nullptr if 'i' is illegal, otherwise return 'elem'.
    void set(UINT i, T elem, SMemPool * pool)
    {
        ASSERTN(is_init(), ("SimpleVector not yet initialized."));
        if (i >= SVEC_elem_num(this)) {
            //grow(i * 2 + 1);
            grow(i + GrowSize, pool);
        }
        m_vec[i] = elem;
        return;
    }
};
//END SimpleVector



//Hash
//
//Hash element recorded not only in hash table but also in Vector,
//which is used in order to speed up accessing hashed elements.
//
//NOTICE:
//    1.T(0) is defined as default nullptr in Hash, so do not use T(0)
//      as element.
//    2.There are four hash function classes are given as default, and
//      if you are going to define you own hash function class, the
//      following member functions you should supply according to your needs.
//        * Return hash-key deduced from 'val'.
//            UINT get_hash_value(OBJTY val) const
//        * Return hash-key deduced from 't'.
//            UINT get_hash_value(T * t) const
//        * Compare t1, t2 when inserting a new element.
//            bool compare(T * t1, T * t2) const
//        * Compare t1, val when inserting a new element.
//            bool compare(T * t1, OBJTY val) const
//    3.Use 'new'/'delete' operator to allocate/free the memory
//      of dynamic object and the virtual function pointers.
#define HC_val(c) (c)->val
#define HC_vec_idx(c) (c)->vec_idx
#define HC_next(c) (c)->next
#define HC_prev(c) (c)->prev
template <class T> struct HC {
    HC<T> * prev;
    HC<T> * next;
    UINT vec_idx;
    T val;
};

#define HB_member(hm) (hm).hash_member
#define HB_count(hm) (hm).hash_member_count
class HashBucket {
public:
    void * hash_member; //hash member list.
    UINT hash_member_count; //the number of member in list.
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
        ASSERTN(sizeof(OBJTY) == sizeof(CHAR*),
                ("exception will taken place in type-cast"));
        return get_hash_value((CHAR const*)v, bucket_size);
    }

    bool compare(CHAR const* s1, CHAR const* s2) const
    { return strcmp(s1, s2) == 0; }

    bool compare(CHAR const* s, OBJTY val) const
    {
        ASSERTN(sizeof(OBJTY) == sizeof(CHAR const*),
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
    COPY_CONSTRUCTOR(Hash);
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
        ASSERTN(m_bucket != nullptr, ("HASH not yet initialized."));
        ASSERT0(m_free_list_pool);
        HC<T> * c = m_free_list.get_free_elem();
        if (c == nullptr) {
            c = (HC<T>*)smpoolMallocConstSize(sizeof(HC<T>), m_free_list_pool);
            ASSERT0(c);
            ::memset((void*)c, 0, sizeof(HC<T>));
        }
        return c;
    }

    //Insert element into hash table.
    //Return true if 't' already exist.
    inline bool insert_v(OUT HC<T> ** bucket_entry, OUT HC<T> ** hc, OBJTY val)
    {
        HC<T> * elemhc = *bucket_entry;
        HC<T> * prev = nullptr;
        while (elemhc != nullptr) {
            ASSERTN(HC_val(elemhc) != T(0),
                    ("Hash element has so far as to be overrided!"));
            if (m_hf.compare(HC_val(elemhc), val)) {
                *hc = elemhc;
                return true;
            }
            prev = elemhc;
            elemhc = HC_next(elemhc);
        } //end while
        elemhc = newhc();

        ASSERTN(elemhc, ("newhc() return nullptr"));
        HC_val(elemhc) = create(val);
        if (prev != nullptr) {
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
    inline bool insert_t(MOD HC<T> ** bucket_entry, OUT HC<T> ** hc, IN T t)
    {
        HC<T> * prev = nullptr;
        HC<T> * elemhc = *bucket_entry;
        while (elemhc != nullptr) {
            ASSERTN(HC_val(elemhc) != T(0), ("Container is empty"));
            if (m_hf.compare(HC_val(elemhc), t)) {
                t = HC_val(elemhc);
                *hc = elemhc;
                return true;
            }
            prev = elemhc;
            elemhc = HC_next(elemhc);
        }

        elemhc = newhc();

        ASSERTN(elemhc, ("newhc() return nullptr"));
        HC_val(elemhc) = t;
        if (prev != nullptr) {
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
        ASSERTN(0, ("Inherited class need to implement"));
        DUMMYUSE(v);
        return T(0);
    }
public:
    Hash(UINT bsize = MAX_SHASH_BUCKET)
    {
        m_bucket = nullptr;
        m_bucket_size = 0;
        m_free_list_pool = nullptr;
        m_elem_count = 0;
        init(bsize);
    }
    virtual ~Hash() { destroy(); }

    //Append 't' into hash table and record its reference into
    //Vector in order to walk through the table rapidly.
    //If 't' already exists, return the element immediately.
    //'find': set to true if 't' already exist.
    //
    //NOTE: Do NOT append 0 to table. Maximum size of T equals sizeof(void*).
    T append(T t, OUT HC<T> ** hct = nullptr, bool * find = nullptr)
    {
        ASSERTN(m_bucket != nullptr, ("Hash not yet initialized."));
        if (t == T(0)) { return T(0); }

        UINT hashv = m_hf.get_hash_value(t, m_bucket_size);
        ASSERTN(hashv < m_bucket_size,
               ("hash value must less than bucket size"));

        HC<T> * elemhc = nullptr;
        if (!insert_t((HC<T>**)&HB_member(m_bucket[hashv]), &elemhc, t)) {
            HB_count(m_bucket[hashv])++;
            m_elem_count++;

            //Get a free slot in elem-vector
            HC_vec_idx(elemhc) = m_elem_vector.get_free_idx();
            m_elem_vector.set(HC_vec_idx(elemhc), t);
            if (find != nullptr) {
                *find = false;
            }
        } else if (find != nullptr) {
            *find = true;
        }

        if (hct != nullptr) {
            *hct = elemhc;
        }
        return HC_val(elemhc);
    }

    //Append 'val' into hash table.
    //More comment see above function.
    //'find': set to true if 't' already exist.
    //
    //NOTE: Do NOT append T(0) to table.
    T append(OBJTY val, OUT HC<T> ** hct = nullptr, bool * find = nullptr)
    {
        ASSERTN(m_bucket != nullptr, ("Hash not yet initialized."));
        UINT hashv = m_hf.get_hash_value(val, m_bucket_size);
        ASSERTN(hashv < m_bucket_size,
                ("hash value must less than bucket size"));

        HC<T> * elemhc = nullptr;
        if (!insert_v((HC<T>**)&HB_member(m_bucket[hashv]), &elemhc, val)) {
            HB_count(m_bucket[hashv])++;
            m_elem_count++;

            //Get a free slot in elem-vector
            HC_vec_idx(elemhc) = m_elem_vector.get_free_idx();
            m_elem_vector.set(HC_vec_idx(elemhc), HC_val(elemhc));
            if (find != nullptr) {
                *find = false;
            }
        } else if (find != nullptr) {
            *find = true;
        }

        if (hct != nullptr) {
            *hct = elemhc;
        }
        return HC_val(elemhc);
    }

    //Count up the memory which hash table used.
    size_t count_mem() const
    {
        size_t count = smpoolGetPoolSize(m_free_list_pool);
        count += m_free_list.count_mem();
        count += m_elem_vector.count_mem();
        count += sizeof(m_elem_count);
        count += sizeof(m_bucket_size);
        count += sizeof(m_bucket);
        count += sizeof(m_hf);
        count += m_bucket_size;
        return count;
    }

    //Clean the data structure but not destroy.
    void clean()
    {
        if (m_bucket == nullptr) { return; }
        ::memset((void*)m_bucket, 0, sizeof(HashBucket) * m_bucket_size);
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
    //    or is VEC_UNDEF.
    T get_first(VecIdx & iter) const
    {
        ASSERTN(m_bucket != nullptr, ("Hash not yet initialized."));
        T t = T(0);
        iter = VEC_UNDEF;
        if (m_elem_count <= 0) { return T(0); }
        VecIdx l = m_elem_vector.get_last_idx();
        for (VecIdx i = 0; i <= l; i++) {
            if ((t = m_elem_vector.get((UINT)i)) != T(0)) {
                iter = i;
                return t;
            }
        }
        return T(0);
    }

    //This function return the next element of given iterator.
    //If it exists, record its index at 'iter' and return the element,
    //otherwise set 'iter' to VEC_UNDEF, and return T(0), where T is the
    //template parameter.
    //
    //'iter': iterator, when the function return, cur will be updated.
    //    If the first element exist, cur is a value that great and equal 0,
    //    or is VEC_UNDEF.
    T get_next(VecIdx & iter) const
    {
        ASSERTN(m_bucket != nullptr && iter >= VEC_UNDEF,
                ("Hash not yet initialized."));
        T t = T(0);
        if (m_elem_count <= 0) { return T(0); }
        VecIdx l = m_elem_vector.get_last_idx();
        for (VecIdx i = iter + 1; i <= l; i++) {
            if ((t = m_elem_vector.get((UINT)i)) != T(0)) {
                iter = i;
                return t;
            }
        }
        iter = VEC_UNDEF;
        return T(0);
    }

    //This function return the last element if it exists, and initialize
    //the iterator, otherwise return T(0), where T is the template parameter.
    //When T is type of integer, return zero may be fuzzy and ambiguous.
    //Invoke get_prev() to get the prev element.
    //
    //'iter': iterator, when the function return, cur will be updated.
    //    If the first element exist, cur is a value that great and equal 0,
    //    or is VEC_UNDEF.
    T get_last(VecIdx & iter) const
    {
        ASSERTN(m_bucket != nullptr, ("Hash not yet initialized."));
        T t = T(0);
        iter = VEC_UNDEF;
        if (m_elem_count <= 0) { return T(0); }
        VecIdx l = m_elem_vector.get_last_idx();
        for (VecIdx i = l; i != VEC_UNDEF; i--) {
            if ((t = m_elem_vector.get((UINT)i)) != T(0)) {
                iter = i;
                return t;
            }
        }
        return T(0);
    }

    //This function return the previous element of given iterator.
    //If it exists, record its index at 'iter' and return the element,
    //otherwise set 'iter' to VEC_UNDEF, and return T(0), where T is the
    //template parameter.
    //
    //'iter': iterator, when the function return, cur will be updated.
    //    If the first element exist, cur is a value that great and equal 0,
    //    or is VEC_UNDEF.
    T get_prev(VecIdx & iter) const
    {
        ASSERTN(m_bucket != nullptr, ("Hash not yet initialized."));
        T t = T(0);
        if (m_elem_count <= 0) { return T(0); }
        for (VecIdx i = iter - 1; i != VEC_UNDEF; i--) {
            if ((t = m_elem_vector.get((UINT)i)) != T(0)) {
                iter = i;
                return t;
            }
        }
        iter = VEC_UNDEF;
        return T(0);
    }

    void init(UINT bsize = MAX_SHASH_BUCKET)
    {
        if (m_bucket != nullptr || bsize == 0) { return; }
        m_bucket = (HashBucket*)::malloc(sizeof(HashBucket) * bsize);
        ::memset((void*)m_bucket, 0, sizeof(HashBucket) * bsize);
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
        if (m_bucket == nullptr) { return; }
        ::free(m_bucket);
        m_bucket = nullptr;
        m_bucket_size = 0;
        m_elem_count = 0;
        m_elem_vector.destroy();
        smpoolDelete(m_free_list_pool);
        m_free_list_pool = nullptr;
    }

    //Dump the distribution of element in hash.
    void dump_intersp(FILE * h) const
    {
        if (h == nullptr) { return; }
        UINT bsize = get_bucket_size();
        HashBucket const* bucket = get_bucket();
        fprintf(h, "\n=== Hash ===");
        for (UINT i = 0; i < bsize; i++) {
            HC<T> * elemhc = (HC<T>*)bucket[i].hash_member;
            fprintf(h, "\nENTRY[%d]:", i);
            while (elemhc != nullptr) {
                fprintf(h, "*");
                elemhc = HC_next(elemhc);
                if (elemhc != nullptr) {
                    //prt(",");
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
    T remove(T t)
    {
        ASSERTN(m_bucket != nullptr, ("Hash not yet initialized."));
        if (t == 0) { return T(0); }

        UINT hashv = m_hf.get_hash_value(t, m_bucket_size);
        ASSERTN(hashv < m_bucket_size,
                ("hash value must less than bucket size"));
        HC<T> * elemhc = (HC<T>*)HB_member(m_bucket[hashv]);
        if (elemhc == nullptr) { return T(0); }
        while (elemhc != nullptr) {
            ASSERTN(HC_val(elemhc) != T(0),
                    ("Hash element has so far as to be overrided!"));
            if (m_hf.compare(HC_val(elemhc), t)) {
                break;
            }
            elemhc = HC_next(elemhc);
        }
        if (elemhc != nullptr) {
            m_elem_vector.set(HC_vec_idx(elemhc), T(0));
            xcom::remove((HC<T>**)&HB_member(m_bucket[hashv]), elemhc);
            m_free_list.add_free_elem(elemhc);
            HB_count(m_bucket[hashv])--;
            m_elem_count--;
            return t;
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
        ASSERTN(m_bucket != nullptr, ("Hash not yet initialized."));
        if (bsize != 0) {
            ASSERT0(bsize > m_bucket_size);
        } else {
            bsize = m_bucket_size * 2;
        }

        HashBucket * new_bucket =
            (HashBucket*)::malloc(sizeof(HashBucket) * bsize);
        ::memset((void*)new_bucket, 0, sizeof(HashBucket) * bsize);
        if (m_elem_count == 0) {
            ::free(m_bucket);
            m_bucket = new_bucket;
            m_bucket_size = bsize;
            return;
        }

        //Free HC containers.
        for (UINT i = 0; i < m_bucket_size; i++) {
            HC<T> * hc = nullptr;
            while ((hc = xcom::removehead((HC<T>**)&HB_member(m_bucket[i])))
                   != nullptr) {
                m_free_list.add_free_elem(hc);
            }
        }

        //Free bucket.
        ::free(m_bucket);

        m_bucket = new_bucket;
        m_bucket_size = bsize;

        //Rehash all elements, and the position in m_elem_vector is unchanged.
        VecIdx l = m_elem_vector.get_last_idx();
        for (VecIdx i = 0; i <= l; i++) {
            T t = m_elem_vector.get((UINT)i);
            if (t == T(0)) { continue; }

            UINT hashv = m_hf.get_hash_value(t, m_bucket_size);
            ASSERTN(hashv < m_bucket_size,
                    ("hash value must less than bucket size"));

            HC<T> * elemhc = nullptr;
            bool doit = insert_t((HC<T>**)&HB_member(m_bucket[hashv]),
                                 &elemhc, t);
            ASSERT0(!doit);
            DUMMYUSE(doit); //to avoid -Werror=unused-variable.

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
        ASSERTN(m_bucket != nullptr, ("Hash not yet initialized."));
        UINT hashv = m_hf.get_hash_value(val, m_bucket_size);
        ASSERTN(hashv < m_bucket_size,
                ("hash value must less than bucket size"));
        HC<T> const* elemhc = (HC<T> const*)HB_member(m_bucket[hashv]);
        if (elemhc != nullptr) {
            while (elemhc != nullptr) {
                ASSERTN(HC_val(elemhc) != T(0),
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
    bool find(T t, HC<T> const** ct = nullptr) const
    {
        ASSERTN(m_bucket != nullptr, ("Hash not yet initialized."));
        if (t == T(0)) { return false; }

        UINT hashv = m_hf.get_hash_value(t, m_bucket_size);
        ASSERTN(hashv < m_bucket_size,
               ("hash value must less than bucket size"));
        HC<T> const* elemhc = (HC<T> const*)HB_member(m_bucket[hashv]);
        while (elemhc != nullptr) {
            ASSERTN(HC_val(elemhc) != T(0),
                   ("Hash element has so far as to be overrided!"));
            if (m_hf.compare(HC_val(elemhc), t)) {
                if (ct != nullptr) {
                    *ct = elemhc;
                }
                return true;
            }
            elemhc = HC_next(elemhc);
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
            ASSERT0(ot != nullptr);
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
    RBGRAY = 3,
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
        parent = nullptr;
        lchild = nullptr;
        rchild = nullptr;
        key = T(0);
        mapped = Ttgt(0);
        color = RBBLACK;
    }
};


template <class T> class CompareKeyBase {
public:
    bool is_less(T t1, T t2) const { return t1 < t2; }
    bool is_equ(T t1, T t2) const { return t1 == t2; }
    T createKey(T t) { return t; }
};

template <class T, class Ttgt, class CompareKey = CompareKeyBase<T> >
class RBT {
    COPY_CONSTRUCTOR(RBT);
protected:
    typedef RBTNode<T, Ttgt> RBTNType;
    bool m_use_outside_pool;
    UINT m_num_of_tn;
    RBTNType * m_root;
    SMemPool * m_pool;
    RBTNType * m_free_list;
    CompareKey m_ck;
protected:
    RBTNType * new_tn()
    {
        RBTNType * p = (RBTNType*)smpoolMallocConstSize(sizeof(RBTNType),
                                                        m_pool);
        ASSERT0(p);
        ::memset((void*)p, 0, sizeof(RBTNType));
        return p;
    }

    inline RBTNType * new_tn(T t, RBCOL c)
    {
        RBTNType * x = xcom::removehead(&m_free_list);
        if (x == nullptr) {
            x = new_tn();
        } else {
            x->lchild = nullptr;
            x->rchild = nullptr;
        }
        x->key = m_ck.createKey(t);
        x->color = c;
        return x;
    }

    void free_rbt(RBTNType * t)
    {
        if (t == nullptr) { return; }
        t->prev = t->next = t->parent = nullptr;
        t->key = T(0);
        t->mapped = Ttgt(0);
        t->color = RBGRAY;
        xcom::insertbefore_one(&m_free_list, m_free_list, t);
    }

    void rleft(RBTNType * x)
    {
        ASSERT0(x->rchild != nullptr);
        RBTNType * y = x->rchild;
        y->parent = x->parent;
        x->parent = y;
        x->rchild = y->lchild;
        if (y->lchild != nullptr) {
            y->lchild->parent = x;
        }
        y->lchild = x;
        if (y->parent == nullptr) {
            m_root = y;
        } else if (y->parent->lchild == x) {
            y->parent->lchild = y;
        } else if (y->parent->rchild == x) {
            y->parent->rchild = y;
        }
    }

    void rright(RBTNType * y)
    {
        ASSERT0(y->lchild != nullptr);
        RBTNType * x = y->lchild;
        x->parent = y->parent;
        y->parent = x;
        y->lchild = x->rchild;
        if (x->rchild != nullptr) {
            x->rchild->parent = y;
        }
        x->rchild = y;
        if (x->parent == nullptr) {
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
        RBTNType * y = nullptr;
        while (z->parent != nullptr && z->parent->color == RBRED) {
            if (is_lchild(z->parent)) {
                y = z->parent->parent->rchild;
                if (y != nullptr && y->color == RBRED) {
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
                continue;
            }

            ASSERT0(is_rchild(z->parent));
            y = z->parent->parent->lchild;
            if (y != nullptr && y->color == RBRED) {
                z->parent->color = RBBLACK;
                z->parent->parent->color = RBRED;
                y->color = RBBLACK;
                z = z->parent->parent;
                continue;
            }
            if (is_lchild(z)) {
                z = z->parent;
                rright(z);
                continue;
            }
            ASSERT0(is_rchild(z));
            z->parent->color = RBBLACK;
            z->parent->parent->color = RBRED;
            rleft(z->parent->parent);
        }
        m_root->color = RBBLACK;
    }
public:
    RBT(SMemPool * pool = nullptr) : m_use_outside_pool(false)
    { m_pool = nullptr; init(pool); }
    ~RBT() { destroy(); }

    void init(SMemPool * pool = nullptr)
    {
        if (is_init()) {
            //RBT has been initialized.
            return;
        }
        if (pool != nullptr) {
            //Regard outside pool as current pool.
            m_pool = pool;
            m_use_outside_pool = true;
        } else {
            ASSERT0(!m_use_outside_pool);
            m_pool = smpoolCreate(sizeof(RBTNType) * 4, MEM_CONST_SIZE);
        }
        m_root = nullptr;
        m_num_of_tn = 0;
        m_free_list = nullptr;
    }
    bool is_empty() const { return get_elem_count() == 0; }

    void destroy()
    {
        if (!is_init()) {
            //RBT has been destroied.
            return;
        }
        if (!m_use_outside_pool) {
            smpoolDelete(m_pool);
        }
        m_use_outside_pool = false;
        m_pool = nullptr;
        m_num_of_tn = 0;
        m_root = nullptr;
        m_free_list = nullptr;
    }
    //Count memory usage for current object.
    size_t count_mem() const
    {
        size_t c = sizeof(m_num_of_tn);
        c += sizeof(m_root);
        c += sizeof(m_pool);
        c += sizeof(m_free_list);
        if (!m_use_outside_pool) {
            c += smpoolGetPoolSize(m_pool);
        }
        return c;
    }

    void clean()
    {
        List<RBTNType*> wl;
        if (m_root != nullptr) {
            wl.append_tail(m_root);
            m_root = nullptr;
        }
        while (wl.get_elem_count() != 0) {
            RBTNType * x = wl.remove_head();
            if (x->rchild != nullptr) {
                wl.append_tail(x->rchild);
            }
            if (x->lchild != nullptr) {
                wl.append_tail(x->lchild);
            }
            free_rbt(x);
        }
        m_num_of_tn = 0;
    }

    UINT get_elem_count() const { return m_num_of_tn; }
    SMemPool * get_pool() { return m_pool; }
    RBTNType * get_root() { return m_root; }
    CompareKey * getCompareKeyObject() { return &m_ck; }

    RBTNType * find_with_key(T keyt) const
    {
        if (m_root == nullptr) { return nullptr; }
        RBTNType * x = m_root;
        while (x != nullptr) {
            if (m_ck.is_equ(keyt, x->key)) {
                return x;
            } else if (m_ck.is_less(keyt, x->key)) {
                x = x->lchild;
            } else {
                x = x->rchild;
            }
        }
        return nullptr;
    }

    inline RBTNType * find_rbtn(T t) const
    {
        if (m_root == nullptr) { return nullptr; }
        return find_with_key(t);
    }

    RBTNType * insert(T t, bool * find)
    {
        ASSERT0(find);
        if (m_root == nullptr) {
            RBTNType * z = new_tn(t, RBBLACK);
            m_num_of_tn++;
            m_root = z;
            *find = false;
            return z;
        }

        RBTNType * mark = nullptr;
        RBTNType * x = m_root;
        while (x != nullptr) {
            mark = x;
            if (m_ck.is_equ(t, x->key)) {
                break;
            } else if (m_ck.is_less(t, x->key)) {
                x = x->lchild;
            } else {
                x = x->rchild;
            }
        }

        if (x != nullptr) {
            ASSERT0(m_ck.is_equ(t, x->key));
            *find = true;
            return x;
        }
        *find = false;

        //Add new.
        RBTNType * z = new_tn(t, RBRED);
        z->parent = mark;
        if (mark == nullptr) {
            //The first node.
            m_root = z;
        } else {
            if (m_ck.is_less(t, mark->key)) {
                mark->lchild = z;
            } else {
                mark->rchild = z;
            }
        }
        ASSERT0(z->lchild == nullptr && z->rchild == nullptr);
        m_num_of_tn++;
        fixup(z);
        return z;
    }
    //Return true if current object has been initialized.
    bool is_init() const { return m_pool != nullptr; }

    RBTNType * find_min(RBTNType * x)
    {
        ASSERT0(x);
        while (x->lchild != nullptr) { x = x->lchild; }
        return x;
    }

    RBTNType * find_succ(RBTNType * x)
    {
        if (x->rchild != nullptr) { return find_min(x->rchild); }
        RBTNType * y = x->parent;
        while (y != nullptr && x == y->rchild) {
            x = y;
            y = y->parent;
        }
        return y;
    }

    inline bool both_child_black(RBTNType * x)
    {
        if (x->lchild != nullptr && x->lchild->color == RBRED) {
            return false;
        }
        if (x->rchild != nullptr && x->rchild->color == RBRED) {
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
                }

                if (bro->rchild == nullptr || bro->rchild->color == RBBLACK) {
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
                continue;
            }

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
            }

            if (bro->lchild == nullptr || bro->lchild->color == RBBLACK) {
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
        x->color = RBBLACK;
    }

    Ttgt remove(T t)
    {
        RBTNType * z = find_rbtn(t);
        if (z == nullptr) { return Ttgt(0); }
        Ttgt mapped = z->mapped;
        remove(z);
        return mapped;
    }

    void remove(RBTNType * z)
    {
        if (z == nullptr) { return; }
        if (m_num_of_tn == 1) {
            ASSERTN(z == m_root, ("z is not the node of tree"));
            ASSERTN(z->rchild == nullptr && z->lchild == nullptr,
                    ("z is the last node"));
            //The mapped object that will be removed is recorded in z.
            free_rbt(z);
            m_num_of_tn--;
            m_root = nullptr;
            return;
        }

        RBTNType * y;
        if (z->lchild == nullptr || z->rchild == nullptr) {
            y = z;
        } else {
            y = find_min(z->rchild);
        }

        RBTNType * x = y->lchild != nullptr ? y->lchild : y->rchild;
        RBTNType holder;
        if (x != nullptr) {
            x->parent = y->parent;
        } else {
            holder.parent = y->parent;
        }

        if (y->parent == nullptr) {
            m_root = x;
        } else if (is_lchild(y)) {
            if (x != nullptr) {
                y->parent->lchild = x;
            } else {
                y->parent->lchild = &holder;
            }
        } else {
            if (x != nullptr) {
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
            if (x != nullptr) {
                rmfixup(x);
            } else {
                rmfixup(&holder);
                if (is_rchild(&holder)) {
                    holder.parent->rchild = nullptr;
                } else {
                    ASSERT0(is_lchild(&holder));
                    holder.parent->lchild = nullptr;
                }
            }
        } else if (holder.parent != nullptr) {
            if (is_rchild(&holder)) {
                holder.parent->rchild = nullptr;
            } else {
                ASSERT0(is_lchild(&holder));
                holder.parent->lchild = nullptr;
            }
        }

        //The mapped object that will be removed might not be recorded in y.
        free_rbt(y);
        m_num_of_tn--;
        return;
    }

    //The function will get the first key-value and mapped object.
    //iter should be clean by caller.
    T get_first(List<RBTNType*> & iter, Ttgt * mapped = nullptr) const
    {
        if (m_root == nullptr) {
            if (mapped != nullptr) { *mapped = Ttgt(0); }
            return T(0);
        }
        iter.append_tail(m_root);
        if (mapped != nullptr) { *mapped = m_root->mapped; }
        return m_root->key;
    }

    //The function will get the next key-value and mapped object.
    //Note if one remove some key-value before invoke the function, it may
    //lead to iterate same key-value multiple times.
    //e.g: given key-value in RBTree [112, 115, 109, 3], if one remove 115
    //while iterating these values, the access order is [112, 115, 109, 3, 3].
    //The last 3 will be accessed twice.
    T get_next(List<RBTNType*> & iter, Ttgt * mapped = nullptr) const
    {
        RBTNType * x = iter.remove_head();
        if (x == nullptr) {
            if (mapped != nullptr) { *mapped = Ttgt(0); }
            return T(0);
        }
        if (x->color == RBGRAY) {
            while ((x = iter.get_head()) != nullptr && x->color == RBGRAY) {
                iter.remove_head();
            }
            if (x == nullptr) {
                if (mapped != nullptr) { *mapped = Ttgt(0); }
                return T(0);
            }
            ASSERT0(x->color != RBGRAY);
        }
        if (x->rchild != nullptr) {
            iter.append_tail(x->rchild);
        }
        if (x->lchild != nullptr) {
            iter.append_tail(x->lchild);
        }
        while ((x = iter.get_head()) != nullptr && x->color == RBGRAY) {
            iter.remove_head();
        }
        if (x == nullptr) {
            if (mapped != nullptr) { *mapped = Ttgt(0); }
            return T(0);
        }
        if (mapped != nullptr) { *mapped = x->mapped; }
        return x->key;
    }
};
//END RBTNode


//TMap Iterator based on Double Linked List.
//This class is used to iterate elements in TMap.
//You should call clean() to initialize the iterator.
template <class Tsrc, class Ttgt>
class TMapIter : public List<RBTNode<Tsrc, Ttgt>*> {
    COPY_CONSTRUCTOR(TMapIter);
public:
    TMapIter() {}

    //Return true if the iteration is at the end.
    bool end() const
    { return List<RBTNode<Tsrc, Ttgt>*>::get_elem_count() == 0; }
};


//TMap Iterator based on Single Linked List.
//This class is used to iterate elements in TMap.
//You should call clean() to initialize the iterator.
template <class Tsrc, class Ttgt>
class TMapIter2 : public SList<RBTNode<Tsrc, Ttgt>*> {
    COPY_CONSTRUCTOR(TMapIter2);
public:
    typedef RBTNode<Tsrc, Ttgt>* Container;

public:
    TMapIter2(SMemPool * pool) : SList<RBTNode<Tsrc, Ttgt>*>(pool)
    { ASSERT0(pool); }

    //Return true if the iteration is at the end.
    bool end() const
    { return SList<RBTNode<Tsrc, Ttgt>*>::get_elem_count() == 0; }
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
//    1. Tsrc(0) is defined as default nullptr in TMap, do NOT use T(0)
//       as element.
//    2. Keep the key *UNIQUE* .
//    3. Overload operator == and operator < if Tsrc is neither basic type
//       nor pointer type.
template <class Tsrc, class Ttgt, class CompareKey = CompareKeyBase<Tsrc> >
class TMap : public RBT<Tsrc, Ttgt, CompareKey> {
    COPY_CONSTRUCTOR(TMap);
public:
    typedef RBT<Tsrc, Ttgt, CompareKey> BaseType;
    typedef RBTNode<Tsrc, Ttgt> RBTNType;
    TMap(SMemPool * pool = nullptr) : RBT<Tsrc, Ttgt, CompareKey>(pool) {}
    ~TMap() {}

    //Append <key, mapped> pair of 'src' to current object.
    void append(TMap<Tsrc, Ttgt, CompareKey> const& src)
    {
        ASSERT0(this != &src);
        TMapIter<Tsrc, Ttgt> iter;
        Ttgt val;
        for (Tsrc key = src.get_first(iter, &val);
             !iter.end(); key = src.get_next(iter, &val)) {
            set(key, val);
        }
    }

    //This function should be invoked if TMap is initialized manually.
    void init(SMemPool * pool = nullptr)
    { RBT<Tsrc, Ttgt, CompareKey>::init(pool); }

    void copy(TMap<Tsrc, Ttgt, CompareKey> const& src)
    {
        ASSERT0(this != &src);
        RBT<Tsrc, Ttgt, CompareKey>::clean();
        append(src);
    }

    //This function should be invoked if TMap is destroied manually.
    void destroy() { RBT<Tsrc, Ttgt, CompareKey>::destroy(); }

    //Always set new object to 't'.
    //The function will enforce mapping between t and mapped object even if
    //'t' has been mapped.
    Tsrc setAlways(Tsrc t, Ttgt mapped)
    {
        bool find = false;
        RBTNType * z = BaseType::insert(t, &find);
        DUMMYUSE(find); //to avoid -Werror=unused-variable.
        ASSERT0(z);
        z->mapped = mapped;
        return z->key; //key may be different with ''t'.
    }

    //Always set new object to 't' if 't' has been inserted into the mapping
    //table.
    Tsrc setIfFind(Tsrc t, Ttgt mapped)
    {
        RBTNType * z = BaseType::find_rbtn(t);
        if (z == nullptr) {
            return Tsrc(0);
        }
        z->mapped = mapped;
        return z->key; //key may be different with ''t'.
    }

    //Establishing mapping in between 't' and 'mapped'.
    //Note this function will check whether 't' has been mapped.
    Tsrc set(Tsrc t, Ttgt mapped)
    {
        bool find = false;
        RBTNType * z = BaseType::insert(t, &find);
        ASSERT0(z);
        ASSERTN(!find, ("already mapped"));
        z->mapped = mapped;
        return z->key; //key may be different with 't'.
    }

    //Get mapped element of 't'. Set find to true if t is already be mapped.
    //Note this function is readonly.
    Ttgt get(Tsrc t, bool * f = nullptr) const
    {
        RBTNType * z = BaseType::find_rbtn(t);
        if (z == nullptr) {
            if (f != nullptr) {
                *f = false;
            }
            return Ttgt(0);
        }

        if (f != nullptr) {
            *f = true;
        }
        return z->mapped;
    }

    //iter should be clean by caller.
    Tsrc get_first(TMapIter<Tsrc, Ttgt> & iter, Ttgt * mapped = nullptr) const
    { return BaseType::get_first(iter, mapped); }

    Tsrc get_next(TMapIter<Tsrc, Ttgt> & iter, Ttgt * mapped = nullptr) const
    { return BaseType::get_next(iter, mapped); }

    bool find(Tsrc t) const
    {
        bool f;
        get(t, &f);
        return f;
    }

    void reinit() { destroy(); init(); }
    Ttgt remove(Tsrc t) { return BaseType::remove(t); }
};
//END TMap


//TTab
//
//NOTICE:
//    1. T(0) is defined as default nullptr in TTab, do not use T(0) as element.
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
class TTabIter : public List<RBTNode<T, T>*> {
    COPY_CONSTRUCTOR(TTabIter);
public:
    TTabIter() {}

    //Return true if the iteration is at the end.
    bool end() const { return List<RBTNode<T, T>*>::get_elem_count() == 0; }
};

template <class T, class CompareKey = CompareKeyBase<T> >
class TTab : public TMap<T, T, CompareKey> {
    COPY_CONSTRUCTOR(TTab);
public:
    TTab() {}

    typedef RBT<T, T, CompareKey> BaseTypeofTMap;
    typedef TMap<T, T, CompareKey> BaseTMap;

    //Add element into table.
    //Note: the element in the table must be unqiue.
    T append(T t)
    {
        ASSERT0(t != T(0));
        #ifdef _DEBUG_
        //CASE: Mapped element may not same with 't'.
        //bool find = false;
        //T mapped = BaseTMap::get(t, &find);
        //if (find) {
        //    ASSERT0(mapped == t);
        //}
        #endif
        return BaseTMap::setAlways(t, t);
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

        return BaseTMap::setAlways(t, t);
    }

    T remove(T t)
    {
        ASSERT0(t != T(0));
        return BaseTMap::remove(t);
    }

    bool find(T t) const { return BaseTMap::find(t); }

    //iter should be clean by caller.
    T get_first(TTabIter<T> & iter) const
    { return BaseTypeofTMap::get_first(iter, nullptr); }

    T get_next(TTabIter<T> & iter) const
    { return BaseTypeofTMap::get_next(iter, nullptr); }
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
//    1. Tsrc(0) is defined as default nullptr in HMap, so do
//       not use T(0) as element.
//    2. The map is implemented base on Hash, and one hash function class
//       have been given.
//
//    3. Must use 'new'/'delete' operator to allocate/free the
//       memory of dynamic object of MAP, because the
//       virtual-function-pointers-table is needed.
template <class Tsrc, class Ttgt, class HF = HashFuncBase<Tsrc> >
class HMap : public Hash<Tsrc, HF> {
    COPY_CONSTRUCTOR(HMap);
protected:
    Vector<Ttgt> m_mapped_elem_table;

    //Find hash container
    HC<Tsrc> * findhc(Tsrc t) const
    {
        if (t == Tsrc(0)) { return nullptr; }
        UINT hashv = Hash<Tsrc, HF>::m_hf.get_hash_value(t,
            Hash<Tsrc, HF>::m_bucket_size);
        ASSERTN((hashv < Hash<Tsrc, HF>::m_bucket_size),
                ("hash value must less than bucket size"));
        HC<Tsrc> * elemhc =
            (HC<Tsrc>*)HB_member((Hash<Tsrc, HF>::m_bucket[hashv]));
        if (elemhc != nullptr) {
            while (elemhc != nullptr) {
                ASSERTN(HC_val(elemhc) != Tsrc(0),
                        ("Hash element has so far as to be overrided!"));
                if (Hash<Tsrc, HF>::m_hf.compare(HC_val(elemhc), t)) {
                    return elemhc;
                }
                elemhc = HC_next(elemhc);
            } //end while
        }
        return nullptr;
    }
public:
    HMap(UINT bsize = MAX_SHASH_BUCKET) : Hash<Tsrc, HF>(bsize)
    { m_mapped_elem_table.init(); }
    virtual ~HMap() { destroy(); }

    //Alway set new mapping even if it has done.
    void setAlways(Tsrc t, Ttgt mapped)
    {
        ASSERTN((Hash<Tsrc, HF>::m_bucket != nullptr), ("not yet initialize."));
        if (t == Tsrc(0)) { return; }
        HC<Tsrc> * elemhc = nullptr;
        Hash<Tsrc, HF>::append(t, &elemhc, nullptr);
        ASSERTN(elemhc != nullptr,
                ("Element does not append into hash table."));
        m_mapped_elem_table.set(HC_vec_idx(elemhc), mapped);
    }

    SMemPool * get_pool() { return Hash<Tsrc, HF>::get_free_list_pool(); }

    //Get mapped pointer of 't'
    Ttgt get(Tsrc t, bool * find = nullptr)
    {
        ASSERTN((Hash<Tsrc, HF>::m_bucket != nullptr),
                ("not yet initialize."));
        HC<Tsrc> * elemhc = findhc(t);
        if (elemhc != nullptr) {
            if (find != nullptr) { *find = true; }
            return m_mapped_elem_table.get(HC_vec_idx(elemhc));
        }
        if (find != nullptr) { *find = false; }
        return Ttgt(0);
    }

    Vector<Ttgt> * get_tgt_elem_vec() { return &m_mapped_elem_table; }

    void clean()
    {
        ASSERTN((Hash<Tsrc, HF>::m_bucket != nullptr), ("not yet initialize."));
        Hash<Tsrc, HF>::clean();
        m_mapped_elem_table.clean();
    }
    //Count memory usage for current object.
    size_t count_mem() const
    {
        size_t count = m_mapped_elem_table.count_mem();
        count += ((Hash<Tsrc, HF>*)this)->count_mem();
        return count;
    }

    void init(UINT bsize = MAX_SHASH_BUCKET)
    {
        //Only do initialization while m_bucket is nullptr.
        Hash<Tsrc, HF>::init(bsize);
        m_mapped_elem_table.init();
    }

    void destroy()
    {
        Hash<Tsrc, HF>::destroy();
        m_mapped_elem_table.destroy();
    }

    //This function iterate mappped elements.
    //Note Ttgt(0) will be served as default nullptr.
    Ttgt get_first_elem(VecIdx & pos) const
    {
        for (VecIdx i = 0; i <= m_mapped_elem_table.get_last_idx(); i++) {
            Ttgt t = m_mapped_elem_table.get((UINT)i);
            if (t != Ttgt(0)) {
                pos = i;
                return t;
            }
        }

        pos = VEC_UNDEF;
        return Ttgt(0);
    }

    //This function iterate mappped elements.
    //Note Ttgt(0) will be served as default nullptr.
    Ttgt get_next_elem(VecIdx & pos) const
    {
        ASSERT0(pos != VEC_UNDEF);
        for (VecIdx i = pos + 1; i <= m_mapped_elem_table.get_last_idx(); i++) {
            Ttgt t = m_mapped_elem_table.get((UINT)i);
            if (t != Ttgt(0)) {
                pos = i;
                return t;
            }
        }

        pos = VEC_UNDEF;
        return Ttgt(0);

    }

    void reinit() { destroy(); init(); }

    //Establishing mapping in between 't' and 'mapped'.
    void set(Tsrc t, Ttgt mapped)
    {
        ASSERTN((Hash<Tsrc, HF>::m_bucket != nullptr), ("not yet initialize."));
        if (t == Tsrc(0)) { return; }

        HC<Tsrc> * elemhc = nullptr;
        Hash<Tsrc, HF>::append(t, &elemhc, nullptr);

        ASSERTN(elemhc != nullptr,
                ("Element does not append into hash table yet."));
        ASSERTN(Ttgt(0) == m_mapped_elem_table.get(HC_vec_idx(elemhc)),
                ("Already be mapped"));
        m_mapped_elem_table.set(HC_vec_idx(elemhc), mapped);
    }

    void setv(OBJTY v, Ttgt mapped)
    {
        ASSERTN((Hash<Tsrc, HF>::m_bucket != nullptr), ("not yet initialize."));
        if (v == 0) { return; }

        HC<Tsrc> * elemhc = nullptr;
        Hash<Tsrc, HF>::append(v, &elemhc, nullptr);
        ASSERTN(elemhc != nullptr,
               ("Element does not append into hash table yet."));
        ASSERTN(Ttgt(0) == m_mapped_elem_table.get(HC_vec_idx(elemhc)),
               ("Already be mapped"));
        m_mapped_elem_table.set(HC_vec_idx(elemhc), mapped);
    }
};
//END MAP


//TMap Iterator based on Double Linked List.
//This class is used to iterate elements in TMap.
//You should call clean() to initialize the iterator.
template <class Tsrc, class Ttgt, class MAP_Tsrc2Ttgt, class MAP_Ttgt2Tsrc>
class DMapIter : public List<RBTNode<Tsrc, Ttgt>*> {
    COPY_CONSTRUCTOR(DMapIter);
public:
    DMapIter() {}
};


//Dual Directional Map
//
//Tsrc2Ttgt: class derive from TMap<Tsrc, Ttgt>
//Ttgt2Tsrc: class derive from TMap<Ttgt, Tsrc>
//
//Usage: Mapping OPND to corresponding OPER.
//    class MAP1 : public TMap<OPND*, OPER*> {
//    public:
//    };
//
//    class MAP2 : public TMap<OPER*, OPND*>{
//    public:
//    };
//
//    DMap<OPND*, OPER*, MAP1, MAP2> opnd2oper_dmap;
//
//NOTICE:
//    1. Tsrc(0) is defined as default nullptr in DMap, so do not use T(0)
//       as element.
//    2. DMap Object's memory can be allocated by malloc() dynamically.
template <class Tsrc, class Ttgt, class Tsrc2Ttgt, class Ttgt2Tsrc>
class DMap {
    COPY_CONSTRUCTOR(DMap);
protected:
    Tsrc2Ttgt m_src2tgt_map;
    Ttgt2Tsrc m_tgt2src_map;
public:
    DMap() {}
    ~DMap() {}

    //Alway overlap the old value by new 'mapped' value.
    void setAlways(Tsrc t, Ttgt mapped)
    {
        if (t == Tsrc(0)) { return; }
        m_src2tgt_map.setAlways(t, mapped);
        m_tgt2src_map.setAlways(mapped, t);
    }
    //Count memory usage for current object.
    size_t count_mem() const
    {
        size_t count = m_src2tgt_map.count_mem();
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
    Ttgt get(Tsrc t) { return m_src2tgt_map.get(t); }

    //Inverse mapping
    Tsrc geti(Ttgt t) { return m_tgt2src_map.get(t); }
};
//END DMap


//Extended Dual Directional Map
//
//Usage: Mapping OPND to corresponding OPER.
//    class MAP1 : public TMap<OPND*, OPER*> {
//    public:
//    };
//
//    class MAP2 : public TMap<OPER*, OPND*>{
//    public:
//    };
//
//    DMapEx<OPND*, OPER*> opnd2oper_dmap;
//
//NOTICE:
//    1. Tsrc(0) is defined as default nullptr in DMapEx, so do not use T(0)
//       as element.
//    2. DMapEx Object's memory can be allocated by malloc() dynamically.
template <class Tsrc, class Ttgt>
class DMapEx : public DMap<Tsrc, Ttgt, TMap<Tsrc, Ttgt>, TMap<Ttgt, Tsrc> > {
    COPY_CONSTRUCTOR(DMapEx);
public:
    typedef TMap<Tsrc, Ttgt> Tsrc2Ttgt;
    typedef TMapIter<Tsrc, Ttgt> Tsrc2TtgtIter;

    typedef TMap<Ttgt, Tsrc> Ttgt2Tsrc;
    typedef TMapIter<Ttgt, Tsrc> Ttgt2TsrcIter;
public:
    DMapEx() {}
    ~DMapEx() {}

    //iter should be clean by caller.
    Tsrc get_first(Tsrc2TtgtIter & iter, Ttgt * mapped = nullptr) const
    {
        return DMap<Tsrc, Ttgt, TMap<Tsrc, Ttgt>, TMap<Ttgt, Tsrc> >::
               m_src2tgt_map.get_first(iter, mapped);
    }

    Tsrc get_next(Tsrc2TtgtIter & iter, Ttgt * mapped = nullptr) const
    {
        return DMap<Tsrc, Ttgt, TMap<Tsrc, Ttgt>, TMap<Ttgt, Tsrc> >::
               m_src2tgt_map.get_next(iter, mapped);
    }
};


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
//    1. Tsrc(0) is defined as default nullptr in MMap, do not use T(0)
//       as element.
//
//    2. MMap allocate memory for 'TAB_Ttgt' and return 'TAB_Ttgt *'
//       when get(Tsrc) be invoked. DO NOT free these objects yourself.
//
//    3. TAB_Ttgt should be pointer type.
//       e.g: Given type of tgt's table is a class that
//            OP_HASH : public Hash<OPER*>,
//            then type MMap<OPND*, OPER*, OP_HASH> is ok, but
//            type MMap<OPND*, OPER*, OP_HASH*> is not expected.
//
//    4. Do not use DMap directly, please overload following
//       functions optionally:
//       * create hash-element container.
//         T * create(OBJTY v)
//
//         e.g: Mapping one OPND to a number of OPERs.
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
    COPY_CONSTRUCTOR(MMap);
protected:
    BYTE m_is_init:1; //To make sure functions are idempotent.
public:
    MMap()
    {
        m_is_init = false;
        init();
    }
    virtual ~MMap() { destroy(); }

    //Count memory usage for current object.
    size_t count_mem() const
    {
        return ((HMap<Tsrc, TAB_Ttgt*, HF>*)this)->count_mem() +
               sizeof(m_is_init);
    }

    void destroy()
    {
        if (!m_is_init) { return; }
        INT iter = 0;
        TAB_Ttgt * tgttab = get(HMap<Tsrc, TAB_Ttgt*, HF>::get_first(iter));
        UINT n = HMap<Tsrc, TAB_Ttgt*, HF>::get_elem_count();
        for (UINT i = 0; i < n; i++) {
            ASSERT0(tgttab);
            delete tgttab;
            tgttab = HMap<Tsrc, TAB_Ttgt*, HF>::
                get(HMap<Tsrc, TAB_Ttgt*, HF>::get_next(iter));
        }

        HMap<Tsrc, TAB_Ttgt*, HF>::destroy();
        m_is_init = false;
    }

    //Get mapped elements of 't'
    TAB_Ttgt * get(Tsrc t) { return HMap<Tsrc, TAB_Ttgt*, HF>::get(t); }

    void init()
    {
        if (m_is_init) { return; }
        HMap<Tsrc, TAB_Ttgt*, HF>::init();
        m_is_init = true;
    }

    void reinit() { destroy(); init(); }

    //Establishing mapping in between 't' and 'mapped'.
    void set(Tsrc t, Ttgt mapped)
    {
        if (t == Tsrc(0)) { return; }
        TAB_Ttgt * tgttab = HMap<Tsrc, TAB_Ttgt*, HF>::get(t);
        if (tgttab == nullptr) {
            //Do NOT use Hash::xmalloc(sizeof(TAB_Ttgt)) to allocate memory,
            //because __vfptr is nullptr. __vfptr points to TAB_Ttgt::vftable.
            tgttab = new TAB_Ttgt;
            HMap<Tsrc, TAB_Ttgt*, HF>::set(t, tgttab);
        }
        tgttab->append_node(mapped);
    }
};

} //namespace xcom
#endif
