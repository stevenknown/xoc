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
#ifndef __FLAG_SET_H__
#define __FLAG_SET_H__

namespace xcom {

typedef BSIdx FlagSetIdx;

//
//START FlagSet
//
template <class FlagDescSeg, UINT FlagDescNum>
class FlagSet : public FixedSizeBitSet {
protected:
    static UINT const g_flagset_byte_size = sizeof(FlagDescSeg) * FlagDescNum;
    BYTE m_flagbuf[g_flagset_byte_size];
public:
    FlagSet() : xcom::FixedSizeBitSet(m_flagbuf, g_flagset_byte_size)
    {
        ASSERT0(g_flagset_byte_size > 0);
        ::memset(m_flagbuf, 0, g_flagset_byte_size);
    }
    FlagSet(FlagDescSeg v) : FixedSizeBitSet(m_flagbuf, g_flagset_byte_size)
    {
        ASSERT0(sizeof(v) <= g_flagset_byte_size);
        ::memcpy(m_flagbuf, &v, sizeof(v));
    }
    FlagSet(BYTE const* vbuf, UINT vbuflen)
        : FixedSizeBitSet(m_flagbuf, g_flagset_byte_size)
    {
        ASSERT0(vbuflen <= g_flagset_byte_size);
        ::memcpy(m_flagbuf, vbuf, vbuflen);
    }

    //The function unifies all flags in 'st' into current flags buffer.
    void bunion(FlagSet const& st) { xcom::FixedSizeBitSet::bunion(st); }
    void bunion(FlagSetIdx i) { xcom::FixedSizeBitSet::bunion(i); }

    void dump(FILE * h) const { FixedSizeBitSet::dump(h); }

    //Return true if current flagset includes 'v', which 'v' may contain
    //combined flags.
    //e.g: if v is 0x5, it indicates v is combined with 0b100 and 0b1.
    bool have(FlagSetIdx v) const { return is_contain(v); }

    //Return the number of flags.
    UINT get_flag_num() const { return get_elem_count(); }

    //Return true if current flagset only includes 'v'.
    bool only_have(FlagSetIdx i) const
    { return have(i) && get_elem_count() == 1; }

    //The function removes flag out of flagset.
    void remove(FlagSetIdx i) { diff(i); }

    //The function adds new flag into flagset.
    void set(FlagSetIdx i) { bunion(i); }
};
//END FlagSet

typedef FlagSet<UINT64, 1> DefFlagSet; //Default Size FlagSet.

} //namespace xcom

#endif //END __FLAG_SET_H__
