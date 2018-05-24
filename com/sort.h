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
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
@*/
#ifndef __SORT_H_
#define __SORT_H_

namespace xcom {

#define NTREE_KIDS 2
template <class T> class NTREE {
public:
    T val;
    UINT id;
    bool is_active; //treenode holds value.
    NTREE * kid[NTREE_KIDS];
    NTREE * parent;

    NTREE()
    {
        is_active = false;
        parent = NULL;
        ::memset((CHAR*)kid, 0, sizeof(NTREE) * NTREE_KIDS);
    }
};
#define NTREE_id(t)             ((t)->id)
#define NTREE_kid(t, n)         ((t)->kid[n])
#define NTREE_parent(t)         ((t)->parent)
#define NTREE_is_active(t)      ((t)->is_active)
#define NTREE_val(t)            ((t)->val)


//
//START Bucket
//
class Bucket : public Hash<float> {
    float m_factor;
public:
    Bucket(UINT elem_num) : Hash<float>(elem_num)
    { m_factor = ((float)1.0) / elem_num; }

    UINT bucket_get_hash_value(float t)
    { return (UINT)(t / m_factor); }

    bool bucket_compare(float elem_in_buck, float elem_input)
    { return elem_input < elem_in_buck; }

    float append(float t);
    void extract_elem(OUT Vector<float> & data);
    void dump();
};


float Bucket::append(float t)
{
    ASSERT(m_is_init, ("Hash not yet initialized."));
    if (t == 0) return 0;

    UINT hashv = bucket_get_hash_value(t);
    HC<float> * elemhc = (HC<float>*)SHASH_MEMBER(m_bucket[hashv]);
    if (elemhc) {
        HC<float> * prev;
        while (elemhc) {
            ASSERT(HC_val(elemhc) != float(0), ("Container is empty"));
            if (bucket_compare(HC_val(elemhc), t)) {
                break;
            }
            prev = elemhc;
            elemhc = elemhc->next;
        }

        HC<float> * new_insert_one = newhc();
        ASSERT(new_insert_one, ("newhc return NULL"));
        HC_val(new_insert_one) = t;

        if (elemhc == NULL) {
            //Append on tail of element-list
            insertafter((HC<float>**)&(SHASH_MEMBER(m_bucket[hashv])),
                        new_insert_one);
        } else {
            //Insert before the larger one to generate increment-list.
            insertbefore_one((HC<float>**)&(SHASH_MEMBER(m_bucket[hashv])),
                             elemhc, new_insert_one);
        }
        SHASH_MEMBER_COUNT(m_bucket[hashv])++;
        m_elem_count++;

        //Get a free slot in elem-vector
        HC_vec_idx(elemhc) = m_elem_vector.get_free_idx();
    } else {
        elemhc = newhc();
        ASSERT(elemhc, ("newhc return NULL"));
        HC_val(elemhc) = t;
        SHASH_MEMBER(m_bucket[hashv]) = elemhc;
        SHASH_MEMBER_COUNT(m_bucket[hashv])++;
        m_elem_count++;
        HC_vec_idx(elemhc) = m_elem_vector.get_free_idx();
    }
    m_elem_vector.set(HC_vec_idx(elemhc),t);
    return t;
}


void Bucket::dump()
{
    INT j = 0;
    printf("\nBUCKET");
    for (UINT i = 0; i < Hash<float>::m_bucket_size; i++) {
        printf("\n\tB%d:", i);
        HC<float> * elemhc = (HC<float>*)SHASH_MEMBER(m_bucket[i]);
        while (elemhc) {
            printf("%f,", HC_val(elemhc));
            elemhc = elemhc->next;
        }
    }
}


void Bucket::extract_elem(OUT Vector<float> & data)
{
    INT j = 0;
    for (UINT i = 0; i < Hash<float>::m_bucket_size; i++) {
        HC<float> * elemhc = (HC<float>*)SHASH_MEMBER(m_bucket[i]);
        while (elemhc) {
            data[j++] = HC_val(elemhc);
            elemhc = elemhc->next;
        }
    }
}
//END Bucket


//Sort
//NOTICE: compare() operator of type 'T' is necessary.
template <class T> class Sort {
    SMemPool * m_pool;
    UINT m_tree_id;

    void _qsort(IN Vector<T> & data, INT first_idx, INT last_idx);
    void _merge_sort(IN Vector<T> const& data,
                     INT start_idx,
                     INT end_idx,
                     OUT Vector<T> & output);
    void _revise_tree(NTREE<T> * t);
    NTREE<T> * _build_heap(IN OUT Vector<T> & data,
                           OUT List<NTREE<T>*> & treenode_list);
    void _min_heapify(NTREE<T> * t);
    bool _bucket_sort_check(IN OUT Vector<T> & data);
    void _insert_sort(IN OUT Vector<T> & data, UINT start_idx, UINT end_idx);

    void * xmalloc(INT size)
    {
        void * p = smpoolMalloc(size, m_pool);
        if (p == NULL) return NULL;
        ::memset(p, 0, size);
        return p;
    }
public:
    Sort() { m_pool = smpoolCreate(64, MEM_COMM); }
    ~Sort() { smpoolDelete(m_pool); }
    void shell_sort(IN OUT Vector<T> & data);
    void bucket_sort(IN OUT Vector<T> & data);
    void counting_sort(IN OUT Vector<T> & data);
    void heap_sort(IN OUT Vector<T> & data);
    void merge_sort(IN OUT Vector<T> & data);
    void bubble_sort(IN OUT Vector<T> & data);
    void qsort(IN OUT Vector<T> & data);
};


template <class T>
void Sort<T>::_qsort(IN Vector<T> & data, INT first_idx, INT last_idx)
{
    if (first_idx >= last_idx || first_idx < 0 || last_idx < 0) {
        return;
    }
    INT mid_idx = (first_idx + last_idx + 1) / 2;
    T first_data = data[first_idx];
    T last_data = data[last_idx];
    T mid_data = data[mid_idx];

    //Choose the mid-one to avoid worst case.
    //At worst case, stack may be bursted.
    T key_val = MIN(MAX(first_data, mid_data), last_data);
    INT key_idx;
    if (key_val == first_data) {
        key_idx = first_idx;
    } else if (key_val == mid_data) {
        key_idx = mid_idx;
    } else {
        key_idx = last_idx;
    }

    INT right_start_idx = last_idx;
    INT left_start_idx = first_idx;
FROM_LEFT:
    //Search from left_start_idx to 'key_idx' - 1
    //to find the larger one and swapping with 'key_val'.
    for (INT i = left_start_idx; i <= key_idx - 1; i++) {
        T v = data.get(i);
        if (v > key_val) {
            //Swapping
            data.set(key_idx, v);
            data.set(i, key_val);
            key_idx = i;
            left_start_idx = key_idx + 1;
            goto FROM_RIGHT;
        }
    }

FROM_RIGHT:
    //Search from right_start_idx to 'key_idx' + 1
    //to find the less one and swapping with 'key_val'.
    for (INT j = right_start_idx; j >= key_idx + 1; j--) {
        T v = data.get(j);
        if (v < key_val) {
            //Swapping
            data.set(key_idx, v);
            data.set(j, key_val);
            key_idx = j;
            right_start_idx = key_idx - 1;
            goto FROM_LEFT;
        }
    }
    _qsort(data, first_idx, key_idx - 1);
    _qsort(data, key_idx + 1, last_idx);
}


//Quick Sort.
//The output data will be ordered increment.
template <class T>
void Sort<T>::qsort(IN OUT Vector<T> & data)
{
    if (data.get_last_idx() < 0 || data.get_last_idx() == 0) {
        return;
    }
    _qsort(data, 0, data.get_last_idx());
}


//Bubble Sort.
//The output data will be ordered increment.
template <class T>
void Sort<T>::bubble_sort(IN OUT Vector<T> & data)
{
    INT n = data.get_last_idx();
    for (INT i = 0; i <= n; i++) {
        for (INT j = i+1; j <= n; j++) {
            if (data.get(i) > data.get(j)) {
                //Swap
                T v(data.get(i));
                data.set(i, data.get(j));
                data.set(j, v);
            }
        }
    }
}


//2-ways merger sort.
template <class T>
void Sort<T>::_merge_sort(IN Vector<T> const& data,
                          INT start_idx,
                          INT end_idx,
                          OUT Vector<T> & output)
{
    if (start_idx > end_idx) {
        return;
    }
    if (start_idx == end_idx) {
        output.set(0, data.get(start_idx));
        return;
    }
    if (start_idx + 1 == end_idx) {
        if (data.get(start_idx) <= data.get(end_idx)) {
            output.set(0, data.get(start_idx));
            output.set(1, data.get(end_idx));
        } else {
            output.set(0, data.get(end_idx));
            output.set(1, data.get(start_idx));
        }
        return;
    } else {
        INT mid_idx = (start_idx + end_idx + 1) / 2;
        Vector<T, 32> left_output;
        Vector<T, 32> right_output;
        _merge_sort(data, start_idx, mid_idx - 1, left_output);
        _merge_sort(data, mid_idx, end_idx, right_output);
        INT lidx = 0, ridx = 0;
        INT llen = mid_idx - start_idx, rlen = end_idx - mid_idx + 1;
        for (INT i = 0; i < llen + rlen; i++) {
            if (left_output.get(lidx) <= right_output.get(ridx)) {
                output.set(i, left_output.get(lidx));
                lidx++;
                if (lidx >= llen) {
                    i++;
                    for (INT j = ridx; j <= right_output.get_last_idx(); j++) {
                        output.set(i, right_output.get(j));
                        i++;
                    }
                    break;
                }
            } else {
                output.set(i, right_output.get(ridx));
                ridx++;
                if (ridx >= rlen) {
                    i++;
                    for (INT j = lidx; j <= left_output.get_last_idx(); j++) {
                        output.set(i, left_output.get(j));
                        i++;
                    }
                    break;
                }
            }
        }
    }
}


//Merge Sort.
//The output data will be ordered increment.
template <class T>
void Sort<T>::merge_sort(IN OUT Vector<T> & data)
{
    Vector<T> output;
    _merge_sort(data, 0, data.get_last_idx(), output);
    data.clone(output);
}


#define SWAP(a, b) do { T v(NTREE_val(a)); \
    NTREE_val(a) = NTREE_val(b); \
    NTREE_val(b) = v; } while (0)
template <class T>
void Sort<T>::_revise_tree(NTREE<T> * t)
{
    NTREE<T> * parent = NTREE_parent(t);
    if (parent == NULL) {
        return;
    }
    if (NTREE_val(t) < NTREE_val(parent)) {
        SWAP(t, parent);
        _revise_tree(parent);
    }
}


//Build min-heap, and construct complete binary tree.
template <class T>
NTREE<T> * Sort<T>::_build_heap(IN OUT Vector<T> & data,
                                OUT List<NTREE<T>*> & treenode_list)
{
    if (data.get_last_idx() < 0) {
        return NULL;
    }
    NTREE<T> * nt = (NTREE<T>*)xmalloc(sizeof(NTREE<T>));
    NTREE_id(nt) = m_tree_id++;
    treenode_list.append_tail(nt);

    bool first = true;
    C<NTREE<T>*> * ct = NULL;
    for (INT pos = 0; pos <= data.get_last_idx(); pos++) {
        NTREE<T> * insert_value;
        if (first) {
            first = false;
            insert_value = treenode_list.get_head(&ct);
        } else {
            insert_value = treenode_list.get_next(&ct);
        }
        for (INT i = 0; i < NTREE_KIDS; i++) {
            NTREE<T> * kid = (NTREE<T>*)xmalloc(sizeof(NTREE<T>));
            NTREE_id(kid) = m_tree_id++;
            treenode_list.append_tail(kid);

            NTREE_kid(insert_value, i) = kid;
            NTREE_parent(kid) = insert_value;
        }
        NTREE_val(insert_value) = data.get(pos);
        NTREE_is_active(insert_value) = 1;
        _revise_tree(insert_value);
    }

    //Find root treenode.
    ASSERT0(NTREE_parent(nt) == NULL);
    #ifdef _DEBUG_
    printf("\n");
    for (NTREE<T> * p = treenode_list.get_head();
         p != NULL; p = treenode_list.get_next()) {
        printf("(T%d,%s,val:%d),",
                NTREE_id(p),
                NTREE_is_active(p)?"act":"no-act",
                NTREE_val(p));
    }
    printf("\n");
    #endif
    return nt;
}


template <class T>
void Sort<T>::_min_heapify(NTREE<T> * t)
{
    ASSERT0(NTREE_KIDS == 2);
    NTREE<T> * left = NTREE_kid(t, 0);
    NTREE<T> * right = NTREE_kid(t, 1);
    ASSERT0(left && right);
    if (NTREE_is_active(left) && NTREE_is_active(right)) {
        T v = MIN(MIN(NTREE_val(t), NTREE_val(left)), NTREE_val(right));
        if (NTREE_val(t) == v) {
            //t already be min-heap.
            return;
        } else if (NTREE_val(left) == v) {
            SWAP(t, left);
            _min_heapify(left);
        } else {
            SWAP(t, right);
            _min_heapify(right);
        }
        return;
    }

    if (!NTREE_is_active(left) && !NTREE_is_active(right)) {
        return;
    }

    if (!NTREE_is_active(left)) {
        if (NTREE_val(t) < NTREE_val(right)) {
            return;
        } else {
            SWAP(t, right);
            _min_heapify(right);
            return;
        }
    } else if (!NTREE_is_active(right)) {
        if (NTREE_val(t) < NTREE_val(left)) {
            return;
        } else {
            SWAP(t, left);
            _min_heapify(left);
            return;
        }
    }
}


//Merge Sort.
//The output data will be ordered increment.
template <class T>
void Sort<T>::heap_sort(IN OUT Vector<T> & data)
{
    m_tree_id = 0;
    List<NTREE<T>*> treenode_list;
    NTREE<T> * root = _build_heap(data, treenode_list);

    UINT i = 0;
    data.clean();
    for (NTREE<T> * t = treenode_list.get_tail();
         t != NULL; t = treenode_list.get_prev()) {
        if (!NTREE_is_active(t)) {
            continue;
        }
        data.set(i++, NTREE_val(root));
        SWAP(root, t);
        NTREE_is_active(t) = false;
        _min_heapify(root);
    }
}


//Counting Sort.
template <class T>
void Sort<T>::counting_sort(IN OUT Vector<T> & data)
{
    Vector<T> c; //for tmp use.
    INT n = data.get_last_idx();
    if (n <= 0) {
        return;
    }

    //
    for (INT i = 0; i <= n; i++) {
        T v = data[i];
        ASSERT(v >= 0, ("key value should be larger than 0"));
        c[v] = c[v] + 1;
    }

    //Calclate the number of element whose value less or equal 'j'.
    for (INT j = 1; j <= c.get_last_idx(); j++) {
        c[j] = c[j - 1] + c[j];
    }

    //Sort..
    Vector<T> res;
    //Utilize relation between value and index.
    for (INT k = n; k >= 0; k--) {
        T v = data[k];
        INT idx = c[v]; //value of each input-data is index in 'c'
        res[idx] = v;
        c[v] = idx - 1;
    }
    data.clone(res);
}


template <class T>
bool Sort<T>::_bucket_sort_check(IN OUT Vector<T> & data)
{
    for (INT i = 0; i <= data.get_last_idx(); i++) {
        T v = data[i];
        ASSERT(v < 1 && v >= 0, ("The range of elem-value should be [0,1)"));
    }
    return true;
}


template <class T>
void Sort<T>::bucket_sort(IN OUT Vector<T> & data)
{
    Bucket bk(data.get_last_idx() + 1);
    ASSERT0(_bucket_sort_check(data));
    for (INT i = 0; i <= data.get_last_idx(); i++) {
        bk.append(data[i]);
    }

    bk.extract_elem(data);
}


template <class T>
void Sort<T>::_insert_sort(IN OUT Vector<T> & data,
                           UINT start_idx, UINT end_idx)
{
    List<T> list;
    for (UINT i = start_idx; i <= end_idx; i++) {
        C<T> * ct = NULL;
        T d = data[i];
        for (T v = list.get_head(&ct); ct; v = list.get_next(&ct)) {
            if (d < v) { //in increment order
                break;
            }
        }
        if (ct == NULL) {
            list.append_tail(d);
        } else {
            list.insert_before(d, ct);
        }
    }

    //Reflush 'data'
    T v = list.get_head();
    for (UINT k = start_idx; k <= end_idx; k++) {
        data[k] = v;
        v = list.get_next();
    }
}


template <class T>
void Sort<T>::shell_sort(IN OUT Vector<T> & data)
{
    INT n = data.get_last_idx() + 1;
    if (n <= 1) {
        return;
    }
    UINT gap = 2;
    UINT rem = 0;
    for (UINT group = n / gap; group >= 1;
         gap *= 2, group = n / gap, rem = n % gap) {
        rem = rem == 0 ? 0 : 1;
        for (UINT i = 0; i < group + rem; i++) {
            _insert_sort(data, i*gap, MIN(((i+1)*gap - 1), (UINT)n - 1));            
        }

        if (group == 1) {
            break;
        }
    }

    if (rem != 0) {
        _insert_sort(data, 0, n - 1);
    }
}

} //namespace xcom
#endif
