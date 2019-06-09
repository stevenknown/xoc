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
#include "ltype.h"
#include "comf.h"
#include "smempool.h"
#include "sstl.h"
#include "sort.h"

namespace xcom {

//
//SATRT Bucket
//
float Bucket::append(float t)
{
    ASSERTN(m_bucket, ("Hash not yet initialized."));
    if (t == 0) { return 0; }

    UINT hashv = bucket_get_hash_value(t);
    HC<float> * elemhc = (HC<float>*)HB_member(m_bucket[hashv]);
    if (elemhc) {
        HC<float> * prev;
        while (elemhc) {
            ASSERTN(HC_val(elemhc) != float(0), ("Container is empty"));
            if (bucket_compare(HC_val(elemhc), t)) {
                break;
            }
            prev = elemhc;
            elemhc = elemhc->next;
        }

        HC<float> * new_insert_one = newhc();
        ASSERTN(new_insert_one, ("newhc return NULL"));
        HC_val(new_insert_one) = t;

        if (elemhc == NULL) {
            //Append on tail of element-list
            insertafter((HC<float>**)&(HB_member(m_bucket[hashv])),
                        new_insert_one);
        } else {
            //Insert before the larger one to generate increment-list.
            insertbefore_one((HC<float>**)&(HB_member(m_bucket[hashv])),
                             elemhc, new_insert_one);
        }
        HB_count(m_bucket[hashv])++;
        m_elem_count++;

        //Get a free slot in elem-vector
        HC_vec_idx(elemhc) = m_elem_vector.get_free_idx();
    } else {
        elemhc = newhc();
        ASSERTN(elemhc, ("newhc return NULL"));
        HC_val(elemhc) = t;
        HB_member(m_bucket[hashv]) = elemhc;
        HB_count(m_bucket[hashv])++;
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
        HC<float> * elemhc = (HC<float>*)HB_member(m_bucket[i]);
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
        HC<float> * elemhc = (HC<float>*)HB_member(m_bucket[i]);
        while (elemhc) {
            data[j++] = HC_val(elemhc);
            elemhc = elemhc->next;
        }
    }
}
//END Bucket

} //namespace xcom
