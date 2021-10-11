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
#include "xcominc.h"

namespace xcom {

UINT g_gap;
UINT g_bucket_sz;

class PerfectStringHashMap;
class PerfectHashFunc {
public:
    PerfectStringHashMap * map_ptr;
public:
    UINT get_hash_value(CHAR const* s, UINT bucket_size) const
    {
        ASSERT0(bucket_size != 0);
        UINT v = 0;
        while (*s++) {
            v += (UINT)(*s);
        }
        //return hash32bit(v) % g_bucket_sz;
        v %= g_bucket_sz;
        v += map_ptr->offset_vec.get(map_ptr->offset_in_vec);
        UINT start_offset = map_ptr->offset_vec.get(map_ptr->offset_in_vec);

        for (; v + start_offset < bucket_size &&
             HB_member(map_ptr->m_bucket[v + start_offset]) != nullptr;
             start_offset++) {
            //Entry has been occupied.

        }

        if (v + start_offset < bucket_size &&
            HB_member(map_ptr->m_bucket[v + start_offset]) == nullptr) {
            //Find avaiable entry.
            map_ptr->offset_vec.set(map_ptr->offset_in_vec, start_offset);
        } else {
            //Failed! Need to expand gap!
            map_ptr->need_expand_gap = true;
        }

        return v;
    }

    UINT get_hash_value(OBJTY v, UINT bucket_size) const
    {
        ASSERT_DUMMYUSE(sizeof(OBJTY) == sizeof(CHAR*),
            ("exception will taken place in type-cast"));
        return get_hash_value((CHAR const*)v, bucket_size);
    }

    bool compare(CHAR const* s1, CHAR const* s2) const
    {
        return strcmp(s1, s2) == 0;
    }

    bool compare(CHAR const* s, OBJTY val) const
    {
        ASSERT_DUMMYUSE(sizeof(OBJTY) == sizeof(CHAR const*),
            ("exception will taken place in type-cast"));
        return (strcmp(s, (CHAR const*)val) == 0);
    }
};


class PerfectStringHashMap : public HMap<CHAR const*, UINT, PerfectHashFunc> {
public:
    xcom::Vector<UINT> offset_vec;
    UINT offset_in_vec;
    bool need_expand_gap;
public:
    PerfectStringHashMap(UINT sz = 0) : HMap<CHAR const*, UINT, PerfectHashFunc>(sz)
    {
        m_hf.map_ptr = this;
    }
    virtual ~PerfectStringHashMap() {}

    void set(CHAR const* string, UINT val)
    {
        HMap<CHAR const*, UINT, PerfectHashFunc>::set(string, val);
    }

    void addAllString(xcom::Vector<CHAR const*> & input)
    {
        for (;;) {
            need_expand_gap = false;
            for (INT i = 0; i <= input.get_last_idx(); i++) {
                offset_in_vec = i;
                set(input.get(i), i);
                if (need_expand_gap) {
                    break;
                }
            }

            if (need_expand_gap) {
                g_gap += 2;
                UINT bucket_sz = g_bucket_sz + g_gap;
                clean();
                init(bucket_sz);
            } else {
                //Find the perfect hash map.
                break;
            }
        }
    }
};


void findPerfectHash(xcom::Vector<CHAR const*> & input)
{
    INT str_num = input.get_last_idx() + 1;
    ASSERT0(str_num > 0);
    g_bucket_sz = str_num + 5;
    g_gap = 10;
    UINT bucket_sz = g_bucket_sz + g_gap; //xcom::getNearestPowerOf2((UINT)g_bucket_sz);
    PerfectStringHashMap pshmap(bucket_sz);
    pshmap.addAllString(input);
    pshmap.dump_intersp(g_tfile);
}


} //namespace xcom


int main()
{
    xcom::Vector<CHAR const*> input;
    UINT j = 0;
    for (UINT i = X_UNDEF + 1; i < X_LAST; i++, j++) {
        input.set(j, g_keyword_info[i].name);
    }
    findPerfectHash(input);
}
