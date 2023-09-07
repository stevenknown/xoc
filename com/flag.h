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
#ifndef __FLAG_H__
#define __FLAG_H__

namespace xcom {

//The class represents flag operations.
//Note the class regards integer 0 as the default UNDEF flag.
template <class T>
class Flag {
    T m_flagset;
public:
    Flag() : m_flagset(T(0)) {}
    Flag(T & v) : m_flagset(v) {}
    void clean() { m_flagset = 0; }

    class Iter {
    public:
        UINT idx;
        Iter() : idx(BS_UNDEF) {}
    };

    T & getFlagSet() { return m_flagset; }
    UINT getFlagSetSize() const { return sizeof(m_flagset); }
    T get_value(UINT idx) const { return T(1<<idx); }
    T get_first_flag(Iter & it) const
    {
        BYTE const* ptr = (BYTE*)&m_flagset;
        UINT size = sizeof(m_flagset) / sizeof(BYTE);
        it.idx = ByteOp::get_first_idx(ptr, size);
        if (it.idx == BS_UNDEF) { return T(0); }
        return get_value(it.idx);
    }
    T get_next_flag(Iter & it) const
    {
        BYTE const* ptr = (BYTE*)&m_flagset;
        UINT size = sizeof(m_flagset) / sizeof(BYTE);
        it.idx = ByteOp::get_next_idx(ptr, size, it.idx);
        if (it.idx == BS_UNDEF) { return T(0); }
        return get_value(it.idx);
    }

    bool end(Iter & it) const { return it.idx == BS_UNDEF; }

    bool have(T v) const { return HAVE_FLAG(m_flagset, v); }
    bool only_have(T v) const { return ONLY_HAVE_FLAG(m_flagset, v); }
    void remove(T v) { REMOVE_FLAG(m_flagset, v); }
    void set(T v) { SET_FLAG(m_flagset, v); }
};

class UFlag : public Flag<UINT> {
public:
    UFlag(UINT v) : Flag<UINT>(v) {}
};

} //namespace xcom
#endif //END __FLAG_H__
