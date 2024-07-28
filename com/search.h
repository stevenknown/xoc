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
#ifndef __SEARCH_H_
#define __SEARCH_H_

namespace xcom {

//
//START BinarySearch
//
template <class T> class BinarySearchCompareBase {
public:
    bool is_less(T t1, T t2) const { return t1 < t2; }
    bool is_great(T t1, T t2) const { return t1 > t2; }
    bool is_equ(T t1, T t2) const { return t1 == t2; }
};


template <class T, class Compare = BinarySearchCompareBase<T> >
class BinarySearch {
    Compare m_comp;
    COPY_CONSTRUCTOR(BinarySearch);
public:
    BinarySearch() {}

    //Return true if found val in data.
    //data: elements sorted in incremental order.
    //val: search val in data.
    //valpos: optional, record the position in data if found val.
    //nearless: optional, record position of the nearest value that is
    //          less-than 'val' if not found 'val' in data.
    //neargreat: optional, record position of the nearest value that is
    //           great-than 'val' if not found 'val' in data.
    bool search(Vector<T> const& data, T val, OUT VecIdx * valpos = nullptr,
                OUT VecIdx * nearless = nullptr,
                OUT VecIdx * neargreat = nullptr);
};


//Return true if found val in array.
//array: elements sorted in incremental order.
//val: search val in array.
//valpos: record the position in array if find val.
//nearless: record the position of nearest value that is less-than 'val' if
//          not found 'val' in array.
//neargreat: record the position of nearest value that is great-than 'va' if
//           not found 'val' in array.
template <class T, class Compare>
bool BinarySearch<T, Compare>::search(Vector<T> const& data, T val,
                                      OUT VecIdx * valpos,
                                      OUT VecIdx * nearless,
                                      OUT VecIdx * neargreat)
{
    if (valpos != nullptr) { *valpos = VEC_UNDEF; }
    VecIdx n = data.get_elem_count();
    if (n == 0) { return false; }
    VecIdx lo = 0;
    VecIdx hi = n - 1;
    VecIdx posless = VEC_UNDEF, posgreat = VEC_UNDEF;
    while (lo <= hi) {
        VecIdx pos = lo + (hi - lo) / 2;
        if (m_comp.is_less(val, data.get(pos))) {
            hi = pos - 1;
            posless = hi;
            posgreat = pos;
            continue;
        }
        if (m_comp.is_great(val, data.get(pos))) {
            lo = pos + 1;
            posless = pos;
            posgreat = lo;
            continue;
        }
        if (valpos != nullptr) { *valpos = pos; }
        return true;
    }
    if (nearless != nullptr) { *nearless = posless; }
    if (neargreat != nullptr) {
        posgreat >= n ? *neargreat = VEC_UNDEF : *neargreat = posgreat;
    }
    return false;
}

} //namespace xcom
#endif
