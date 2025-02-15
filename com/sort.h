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

//
//START Bucket
//
template <class T> class Bucket : public Hash<T> {
    float m_factor;
public:
    Bucket(UINT elem_num) : Hash<T>(elem_num)
    { m_factor = ((float)1.0) / elem_num; }

    UINT bucket_get_hash_value(T t)
    { return (UINT)(t / m_factor); }

    bool bucket_compare(T elem_in_buck, T elem_input)
    { return elem_input < elem_in_buck; }

    T append(T t);
    void extract_elem(OUT Vector<T> & data);
    void dump();
};


template <class T>
T Bucket<T>::append(T t)
{
    ASSERTN(Hash<T>::m_bucket, ("Hash not yet initialized."));
    if (t == 0) { return 0; }

    UINT hashv = bucket_get_hash_value(t);
    HC<T> * elemhc = (HC<T>*)HB_member(Hash<T>::m_bucket[hashv]);
    if (elemhc != nullptr) {
        while (elemhc != nullptr) {
            ASSERTN(HC_val(elemhc) != T(0), ("Container is empty"));
            if (bucket_compare(HC_val(elemhc), t)) {
                break;
            }
            elemhc = elemhc->next;
        }

        HC<T> * new_insert_one = Hash<T>::newhc();
        ASSERTN(new_insert_one, ("newhc return nullptr"));
        HC_val(new_insert_one) = t;

        if (elemhc == nullptr) {
            //Append on tail of element-list
            insertafter((HC<T>**)&(HB_member(Hash<T>::m_bucket[hashv])),
                        new_insert_one);
        } else {
            //Insert before the larger one to generate increment-list.
            insertbefore_one((HC<T>**)&(HB_member(
                Hash<T>::m_bucket[hashv])), elemhc, new_insert_one);
        }
        HB_count(Hash<T>::m_bucket[hashv])++;
        Hash<T>::m_elem_count++;

        //Get a free slot in elem-vector
        HC_vec_idx(elemhc) = Hash<T>::m_elem_vector.get_free_idx();
    } else {
        elemhc = Hash<T>::newhc();
        ASSERTN(elemhc, ("newhc return nullptr"));
        HC_val(elemhc) = t;
        HB_member(Hash<T>::m_bucket[hashv]) = elemhc;
        HB_count(Hash<T>::m_bucket[hashv])++;
        Hash<T>::m_elem_count++;
        HC_vec_idx(elemhc) = Hash<T>::m_elem_vector.get_free_idx();
    }
    Hash<T>::m_elem_vector.set(HC_vec_idx(elemhc), t);
    return t;
}


template <class T>
void Bucket<T>::extract_elem(OUT Vector<T> & data)
{
    INT j = 0;
    for (UINT i = 0; i < Hash<T>::m_bucket_size; i++) {
        HC<T> * elemhc = (HC<T>*)HB_member(Hash<T>::m_bucket[i]);
        while (elemhc != nullptr) {
            data[j++] = HC_val(elemhc);
            elemhc = elemhc->next;
        }
    }
}


template <class T>
void Bucket<T>::dump()
{
    printf("\nBUCKET");
    for (UINT i = 0; i < Hash<T>::m_bucket_size; i++) {
        printf("\n\tB%ud:", i);
        HC<T> * elemhc = (HC<T>*)HB_member(Hash<T>::m_bucket[i]);
        while (elemhc != nullptr) {
            printf("%f,", HC_val(elemhc));
            elemhc = elemhc->next;
        }
    }
}
//END Bucket


template <class T> class HeapSort {
public:
    template <class U> class HeapValVector : public Vector<U> {
        U const operator[](UINT index) const;
        U & operator[](UINT index);

        Vector<U> * m_data;
    public:
        HeapValVector(Vector<U> & data) { m_data = &data; }

        //Return the first index in vector of legal heap value.
        UINT get_begin_idx() const { return 1; }
        UINT get_end_idx() const { return 1 + get_elem_count() - 1; }
        U get(UINT idx)
        {
            ASSERTN(idx > 0, ("head value start from 1"));
            return m_data->get(idx - 1);
        }
        UINT get_elem_count() const { return m_data->get_elem_count(); }

        void set(UINT idx, U val)
        {
            ASSERTN(idx > 0, ("head value start from 1"));
            return m_data->set(idx - 1, val);
        }
    };
private:
    //Reorder value in 'data' to build min-heap.
    void build_max_heap(MOD Vector<T> & data)
    {
        HeapValVector<T> hdata(data);
        for (UINT i = last_parent(hdata.get_end_idx());
             i >= hdata.get_begin_idx(); i--) {
            max_heapify(hdata, i, hdata.get_end_idx());
        }
    }

    //Reorder value in 'data' to build min-heap.
    void build_min_heap(MOD Vector<T> & data)
    {
        HeapValVector<T> hdata(data);
        for (UINT i = last_parent(hdata.get_end_idx());
             i >= hdata.get_begin_idx(); i--) {
            min_heapify(hdata, i, hdata.get_end_idx());
        }
    }

    //The floor of integer division.
    UINT parent(UINT i) { return i / 2; }
    UINT lchild(UINT i) { return i * 2; }
    UINT rchild(UINT i) { return i * 2 + 1; }

    //The last (len/2)+1 nodes are kid.
    //The floor of integer division.
    UINT last_parent(UINT len) { return len / 2; }

    void min_heapify(MOD HeapValVector<T> & data, UINT start, UINT end)
    {
        ASSERT0(end > 0);
        UINT l = lchild(start);
        UINT r = rchild(start);
        UINT minor = l <= end ? l : 0;
        if (r <= end && minor != 0) {
            minor = GreatThan(data.get(l), data.get(r)) ? r : l;
        }
        if (minor == 0 || //no kid
            //already be min-heap if start <= minor.
            LessEqualThan(data.get(start), data.get(minor))) {
            return;
        }
        swap(data, start, minor);
        min_heapify(data, minor, end);
    }

    void max_heapify(MOD HeapValVector<T> & data, UINT start, UINT end)
    {
        ASSERT0(start > 0 && end > 0);
        UINT l = lchild(start);
        UINT r = rchild(start);
        ASSERT0(l > start && r > start);
        UINT major = l <= end ? l : 0;
        if (r <= end && major != 0) {
            major = LessThan(data.get(l), data.get(r)) ? r : l;
        }
        if (major == 0 || //no kid
            //already be max-heap if start >= major.
            GreatEqualThan(data.get(start), data.get(major))) {
            return;
        }
        swap(data, start, major);
        max_heapify(data, major, end);
    }

    void swap(MOD HeapValVector<T> & data, UINT i, UINT j)
    {
        INT tmpval = data.get(i);
        data.set(i, data.get(j));
        data.set(j, tmpval);
    }
protected:
    virtual bool GreatThan(T a, T b) const { return a > b; }
    virtual bool GreatEqualThan(T a, T b) const { return a >= b; }
    virtual bool LessThan(T a, T b) const { return a < b; }
    virtual bool LessEqualThan(T a, T b) const { return a <= b; }
public:
    virtual ~HeapSort() {}

    //HeapSort.
    //The output data will be ordered incrementally.
    void sort(MOD Vector<T> & data)
    {
        build_max_heap(data);
        HeapValVector<T> hdata(data);
        UINT end = hdata.get_end_idx();
        swap(hdata, hdata.get_begin_idx(), end);
        end--;
        for (UINT i = hdata.get_begin_idx(); i <= end; end--) {
            max_heapify(hdata, i, end);
            swap(hdata, i, end);
        }
    }
};


template <class T> class QuickSort {
    void _qsort(Vector<T> & data, INT first_idx, INT last_idx)
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
        T key_val = _min(_max(first_data, mid_data), last_data);
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
            if (GreatThan(v, key_val)) {
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
            if (LessThan(v, key_val)) {
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
protected:
    //The function is optional to sorting, usually used in calibrating
    //mid-value to avoid degrdating to the worst case.
    //e.g:you can return 'a' directly.
    virtual T _max(T a, T b) const { return MAX(a, b); }

    //The function is optional to sorting, usually used in calibrating
    //mid-value to avoid degrdating to the worst case.
    //e.g:you can return 'a' directly.
    virtual T _min(T a, T b) const { return MIN(a, b); }

    //The function is necessary to sorting.
    //Return true if 'a' > 'b'.
    virtual bool GreatThan(T a, T b) const { return a > b; }

    //The function is necessary to sorting.
    //Return true if 'a' < 'b'.
    virtual bool LessThan(T a, T b) const { return a < b; }
public:
    virtual ~QuickSort() {}

    //Quick Sort.
    //The output data will be ordered incrementally.
    void sort(MOD Vector<T> & data)
    {
        if (data.get_last_idx() == VEC_UNDEF || data.get_last_idx() == 0) {
            return;
        }
        _qsort(data, 0, data.get_last_idx());
    }
};


template <class T> class MergeSort {
    //2-ways Merge Sort.
    void _merge_sort(Vector<T> const& data, VecIdx start_idx, VecIdx end_idx,
                     OUT Vector<T> & output)
    {
        if (start_idx > end_idx) { return; }
        if (start_idx == end_idx) {
            output.set(0, data.get(start_idx));
            return;
        }
        if (start_idx + 1 == end_idx) {
            if (LessEqualThan(data.get(start_idx), data.get(end_idx))) {
                output.set(0, data.get(start_idx));
                output.set(1, data.get(end_idx));
                return;
            }
            output.set(0, data.get(end_idx));
            output.set(1, data.get(start_idx));
            return;
        }
        VecIdx mid_idx = (start_idx + end_idx + 1) / 2;
        Vector<T> left_output;
        Vector<T> right_output;
        _merge_sort(data, start_idx, mid_idx - 1, left_output);
        _merge_sort(data, mid_idx, end_idx, right_output);
        VecIdx lidx = 0, ridx = 0;
        VecIdx llen = mid_idx - start_idx, rlen = end_idx - mid_idx + 1;
        for (INT i = 0; i < llen + rlen; i++) {
            if (LessEqualThan(left_output.get(lidx), right_output.get(ridx))) {
                output.set(i, left_output.get(lidx));
                lidx++;
                if (lidx < llen) { continue; }
                i++;
                for (VecIdx j = ridx; j <= right_output.get_last_idx();
                     j++) {
                    output.set(i, right_output.get(j));
                    i++;
                }
                break;
            }
            output.set(i, right_output.get(ridx));
            ridx++;
            if (ridx < rlen) { continue; }
            i++;
            for (VecIdx j = lidx; j <= left_output.get_last_idx(); j++) {
                output.set(i, left_output.get(j));
                i++;
            }
            break;
        }
    }
protected:
    virtual bool LessEqualThan(T a, T b) const { return a <= b; }
public:
    virtual ~MergeSort() {}

    //Merge Sort.
    //The output data will be ordered incrementally.
    void sort(MOD Vector<T> & data)
    {
        Vector<T> output;
        _merge_sort(data, 0, data.get_last_idx(), output);
        data.copy(output);
    }
};


//Bubble Sort.
//The output data will be ordered incrementally.
template <class T> class BubbleSort {
protected:
    virtual bool GreatThan(T a, T b) const { return a > b; }
public:
    virtual ~BubbleSort() {}

    //Bubble Sort.
    //The output data will be ordered incrementally.
    void sort(MOD Vector<T> & data)
    {
        VecIdx n = data.get_last_idx();
        for (VecIdx i = 0; i <= n; i++) {
            for (VecIdx j = i+1; j <= n; j++) {
                if (GreatThan(data.get(i), data.get(j))) {
                    //Swap
                    T v(data.get(i));
                    data.set(i, data.get(j));
                    data.set(j, v);
                }
            }
        }
    }
};


template <class T> class CountingSort {
public:
    //Counting Sort.
    //The output data will be ordered incrementally.
    void sort(MOD Vector<T> & data)
    {
        Vector<T> c; //for tmp use.
        VecIdx n = data.get_last_idx();
        if (n <= 0) { return; }
        for (VecIdx i = 0; i <= n; i++) {
            T v = data[i];
            ASSERTN(v >= 0, ("key value should be larger than 0"));
            c[v] = c[v] + 1;
        }

        //Calclate the number of element whose value less or equal 'j'.
        for (VecIdx j = 1; j <= c.get_last_idx(); j++) {
            c[j] = c[j - 1] + c[j];
        }

        //Sort.
        Vector<T> res;
        //Utilize relation between value and index.
        for (VecIdx k = n; !IS_VECUNDEF(k); k--) {
            T v = data[k];
            INT idx = c[v]; //value of each input-data is index in 'c'
            res[idx] = v;
            c[v] = idx - 1;
        }
        data.copy(res);
    }
};


template <class T> class BucketSort {
    bool _bucket_sort_check(MOD Vector<T> & data)
    {
        for (VecIdx i = 0; i <= data.get_last_idx(); i++) {
            T v = data[i];
            ASSERTN(v < 1 && v >= 0,
                    ("The range of elem-value should be [0,1)"));
        }
        return true;
    }
public:
    //Bucket Sort.
    //The output data will be ordered incrementally.
    void sort(MOD Vector<T> & data)
    {
        Bucket<T> bk(data.get_elem_count());
        ASSERT0(_bucket_sort_check(data));
        for (VecIdx i = 0; i <= data.get_last_idx(); i++) {
            bk.append(data[i]);
        }
        bk.extract_elem(data);
    }
};


template <class T> class ShellSort {
    void _insert_sort(MOD Vector<T> & data, UINT start_idx, UINT end_idx)
    {
        List<T> list;
        for (UINT i = start_idx; i <= end_idx; i++) {
            C<T> * ct = nullptr;
            T d = data[i];
            for (T v = list.get_head(&ct); ct; v = list.get_next(&ct)) {
                if (LessThan(d, v)) { //in incremental order
                    break;
                }
            }
            if (ct == nullptr) {
                list.append_tail(d);
                continue;
            }
            list.insert_before(d, ct);
        }

        //Reflush 'data'
        T v = list.get_head();
        for (UINT k = start_idx; k <= end_idx; k++) {
            data[k] = v;
            v = list.get_next();
        }
    }
protected:
    virtual bool LessThan(T a, T b) const { return a < b; }
public:
    virtual ~ShellSort() {}

    //Shell Sort.
    //The output data will be ordered incrementally.
    void sort(MOD Vector<T> & data)
    {
        VecIdx n = data.get_elem_count();
        if (n <= 1) { return; }
        UINT gap = 2;
        UINT rem = 0;
        for (UINT group = n / gap; group >= 1;
             gap *= 2, group = n / gap, rem = n % gap) {
            rem = rem == 0 ? 0 : 1;
            for (UINT i = 0; i < group + rem; i++) {
                _insert_sort(data, i*gap, MIN(((i+1)*gap - 1), (UINT)n - 1));
            }
            if (group == 1) { break; }
        }
        if (rem != 0) {
            _insert_sort(data, 0, n - 1);
        }
    }
};

} //namespace xcom
#endif
