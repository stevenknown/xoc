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
#include "xcominc.h"

namespace xcom {

#define BS_DEF_DUMP_FILE "zbs.log"

//
//START BitSet
//
void * BitSet::realloc(IN void * src, size_t orgsize, size_t newsize)
{
    if (orgsize >= newsize) {
        clean();
        return src;
    }
    void * p = ::malloc(newsize);
    if (src != nullptr) {
        ASSERT0(orgsize > 0);
        ::memcpy(p, src, orgsize);
        ::free(src);
        ::memset((void*)(((BYTE*)p) + orgsize), 0, newsize - orgsize);
    } else {
        ::memset((void*)p, 0, newsize);
    }
    return p;
}


//Allocate bytes
void BitSet::alloc(UINT size)
{
    m_size = size;
    if (m_ptr != nullptr) { ::free(m_ptr); }
    if (size != 0) {
        m_ptr = (BYTE*)::malloc(size);
        ::memset((void*)m_ptr, 0, m_size);
    } else {
        m_ptr = nullptr;
    }
}


//Returns a new set which is the union of set1 and set2,
//and modify set1 as result operand.
void BitSet::bunion(BitSet const& bs)
{
    ASSERT0(this != &bs);
    if (bs.m_ptr == nullptr) { return; }
    UINT cp_sz = bs.m_size; //size needs to unifiy
    if (m_size < bs.m_size) {
        //src's last byte pos.
        BSIdx l = bs.get_last();
        if (l == BS_UNDEF) { return; }
        cp_sz = (UINT)(l / BITS_PER_BYTE) + 1;
        if (m_size < cp_sz) {
            m_ptr = (BYTE*)realloc(m_ptr, m_size, cp_sz);
            m_size = cp_sz;
        }
    }
    UINT const num_of_unit = cp_sz / BYTES_PER_UNIT; //floor-div.
    ASSERTN(m_ptr, ("not yet init"));
    BSUNIT * uint_ptr_this = (BSUNIT*)m_ptr;
    BSUNIT * uint_ptr_bs = (BSUNIT*)bs.m_ptr;
    for (UINT i = 0; i < num_of_unit; i++) {
        uint_ptr_this[i] |= uint_ptr_bs[i];
    }
    for (UINT i = num_of_unit * BYTES_PER_UNIT; i < cp_sz; i++) {
        m_ptr[i] |= bs.m_ptr[i];
    }
}


//Add a element which corresponding to 'elem' bit, and set this bit.
void BitSet::bunion(BSIdx elem)
{
    UINT const first_byte = DIVBPB(elem);
    if (m_size < (first_byte + 1)) {
        m_ptr = (BYTE*)realloc(m_ptr, m_size, first_byte + 1);
        m_size = first_byte+1;
    }
    elem = MODBPB(elem);
    m_ptr[first_byte] |= (BYTE)(1 << elem);
}


//The difference operation calculates the elements that
//distinguish one set from another.
//Remove a element which map with 'elem' bit, and clean this bit.
void BitSet::diff(BSIdx elem)
{
    UINT first_byte = DIVBPB(elem);
    if ((first_byte + 1) > m_size) {
        return;
    }
    elem = MODBPB(elem);
    ASSERTN(m_ptr != nullptr, ("not yet init"));
    m_ptr[first_byte] &= (BYTE)(~(1 << elem));
}


//The difference operation calculates the elements that
//distinguish one set from another.
//Subtracting set2 from set1
//Returns a new set which is
//  { x : member( x, 'set1' ) & ~ member( x, 'set2' ) }.
void BitSet::diff(BitSet const& bs)
{
    ASSERT0(this != &bs);
    if (m_size == 0 || bs.m_size == 0) { return; }
    UINT minsize = MIN(m_size, bs.m_size);
    //Common part: copy the inverse bits.
    //for (UINT i = 0; i < minsize; i++) {
    //    m_ptr[i] &= ~bs.m_ptr[i];
    //}
    UINT num_of_unit = minsize / BYTES_PER_UNIT;
    BSUNIT * uint_ptr_this = (BSUNIT*)m_ptr;
    BSUNIT * uint_ptr_bs = (BSUNIT*)bs.m_ptr;
    for (UINT i = 0; i < num_of_unit; i++) {
        UINT d = uint_ptr_bs[i];
        if (d != 0) {
            uint_ptr_this[i] &= ~d;
        }
    }

    ASSERTN(m_ptr != nullptr, ("not yet init"));
    for (UINT i = num_of_unit * BYTES_PER_UNIT; i < minsize; i++) {
        BYTE d = bs.m_ptr[i];
        if (d != 0) {
            m_ptr[i] = (BYTE)(m_ptr[i] & ~d);
        }
    }
}


//Returns the a new set which is intersection of 'set1' and 'set2'.
void BitSet::intersect(BitSet const& bs)
{
    ASSERT0(this != &bs);
    if (m_ptr == nullptr) { return; }
    if (m_size > bs.m_size) {
        for (UINT i = 0; i < bs.m_size; i++) {
            m_ptr[i] &= bs.m_ptr[i];
        }
        ::memset((void*)(m_ptr + bs.m_size), 0, m_size - bs.m_size);
    } else {
        for (UINT i = 0; i < m_size; i++) {
            m_ptr[i] &= bs.m_ptr[i];
        }
    }
}


//Reverse each bit.
//e.g: 1001 to 0110
//'last_bit_pos': start at 0, e.g:given '101', last bit pos is 2.
void BitSet::rev(BSIdx last_bit_pos)
{
    ASSERTN(m_ptr != nullptr, ("can not reverse empty set"));
    ASSERT0(last_bit_pos != BS_UNDEF);
    UINT const last_byte_pos = ((BSUNIT)last_bit_pos) / (BSUNIT)BITS_PER_BYTE;
    ASSERT0(last_byte_pos < m_size);

    UINT const n = (BSUNIT)last_byte_pos / (BSUNIT)BYTES_PER_UNIT;
    for (UINT i = 0; i < n; i++) {
        ((BSUNIT*)m_ptr)[i] = ~(((BSUNIT*)m_ptr)[i]);
    }

    for (UINT i = n * BYTES_PER_UNIT; i < last_byte_pos; i++) {
        m_ptr[i] = (BYTE)~m_ptr[i];
    }

    //Here we use UINT type to avoid byte truncate operation.
    UINT last_byte = m_ptr[last_byte_pos];

    UINT const mask = (1 << (((BSUNIT)last_bit_pos) % BITS_PER_BYTE + 1)) - 1;
    UINT const rev_last_byte = (~last_byte) & mask;
    last_byte = (last_byte & (~mask)) | rev_last_byte;

    //Truncate to BYTE.
    m_ptr[last_byte_pos] = (BYTE)last_byte;
}


//Complement set of s = univers - s.
void BitSet::complement(BitSet const& univers)
{
    BitSet tmp(univers);
    tmp.diff(*this);
    copy(tmp);
}


//Return the element count in 'set'
//Add up the population count of each byte in the set.  We get the
//population counts from the table above.  Great for a machine with
//effecient loadbyte instructions.
UINT BitSet::get_elem_count() const
{
    if (m_ptr == nullptr) { return 0; }
    UINT count = 0;
//#define HAMMING_WEIGHT_METHOD
#ifdef HAMMING_WEIGHT_METHOD
    ASSERT0(BYTES_PER_UNIT == 4);
    UINT const m = m_size / BYTES_PER_UNIT;
    for (UINT i = 0; i < m; i++) {
        BSUNIT v = ((BSUNIT*)(m_ptr))[i];
        v = v - ((v >> 1) & 0x55555555);
        v = (v & 0x33333333) + ((v >> 2) & 0x33333333);
        count += (((v + (v >> 4)) & 0x0F0F0F0F) * 0x01010101) >> 24;
    }
    for (UINT i = m * BYTES_PER_UNIT; i < m_size; i++) {
        count += g_bit_count[m_ptr[i]];
    }
#else
    count = ByteOp::get_elem_count(m_ptr, m_size);
#endif
    return count;
}


bool BitSet::is_equal(BitSet const& bs) const
{
    ASSERT0(this != &bs);
    UINT size1 = m_size , size2 = bs.m_size;
    if (size1 == 0) {
        if (size2 == 0) { return true; }
        return bs.is_empty();
    }
    if (size2 == 0) {
        if (size1 == 0) { return true; }
        return is_empty();
    }

    BYTE const* ptr1 = m_ptr;
    BYTE const* ptr2 = bs.m_ptr;
    //To guarantee set1 is the smaller.
    if (size1 > size2) {
        BYTE const* tmp = ptr1;
        ptr1 = ptr2;
        ptr2 = tmp;

        UINT tmp1 = size1;
        size1 = size2;
        size2 = tmp1;
    }

    BSUNIT v1 = *(BSUNIT*)ptr1;
    BSUNIT v2 = *(BSUNIT*)ptr2;
    switch (size1) {
    case 1:
        if ((v1&0xff) != (v2&0xff)) { return false; }
        break;
    case 2:
        if ((v1&0xffff) != (v2&0xffff)) { return false; }
        break;
    case 3:
        if ((v1&0xffffff) != (v2&0xffffff)) { return false; }
        break;
    case 4:
        if ((v1&0xffffffff) != (v2&0xffffffff)) { return false; }
        break;
    default:
        if (::memcmp(ptr1, ptr2, size1) != 0) { return false; }
    }

    for (UINT i = size1; i < size2; i++) {
        if (ptr2[i] != 0) {
            return false;
        }
    }
    return true;
}


//Return true if this contain elem.
bool BitSet::is_contain(BSIdx elem) const
{
    if (m_ptr == nullptr) { return false; }
    if (((BSUNIT)elem) >= (MULBPB(m_size))) {
        return false;
    }
    if ((m_ptr[DIVBPB(elem)] & (1 << (MODBPB(elem)))) != 0) {
        return true;
    }
    return false;
}


//Return true if 'this' contains 'bs'.
//'strict': If it is false, we say the bitset contains bs;
//if it is true, the bitset must have at least one
//element that does not belong to 'bs'.
bool BitSet::is_contain(BitSet const& bs, bool strict) const
{
    ASSERT0(this != &bs);
    bool scon = false; //Set to true if 'this' strictly contained 'bs'.
    BSIdx const first_bit = get_first();
    if (first_bit == BS_UNDEF) {
        return false;
    }
    BSIdx const bs_first_bit = bs.get_first();
    if (bs_first_bit == BS_UNDEF) {
        //nullptr set be contained for any set.
        return true;
    }

    if (first_bit > bs_first_bit) {
        return false;
    } else if (strict && first_bit < bs_first_bit) {
        scon = true;
    }

    BSIdx const start_bit = MAX(first_bit, bs_first_bit);
    UINT const start_byte = DIVBPB(start_bit);

    if (m_size < bs.m_size) {//'this' is smaller
        if (bs.get_next(MULBPB(m_size) - 1) != BS_UNDEF) {
            //'bs' has more elements than 'this'.
            return false;
        }
    } else if (strict && m_size > bs.m_size) {//'bs' is smaller
        if (get_next(MULBPB(bs.m_size) - 1) != BS_UNDEF) {
            //'this' has more elements than 'this'.
            scon = true;
        }
    }

    UINT const minsize = MIN(m_size, bs.m_size);
    for (UINT i = start_byte; i < minsize; i++) {
        if ((m_ptr[i] & bs.m_ptr[i]) != bs.m_ptr[i]) {
            return false;
        }
        if (strict && m_ptr[i] != bs.m_ptr[i]) {
            scon = true;
        }
    }

    if (strict && !scon) {
        return false;
    }
    return true;
}


bool BitSet::is_empty() const
{
    if (m_ptr == nullptr) { return true; }
    UINT num_of_unit = m_size / BYTES_PER_UNIT;
    BSUNIT * uint_ptr = (BSUNIT*)m_ptr;
    for (UINT i = 0; i < num_of_unit; i++) {
        if (uint_ptr[i] != 0) {
             return false;
        }
    }
    for (UINT i = num_of_unit * BYTES_PER_UNIT; i < m_size; i++) {
        if (m_ptr[i] != (BYTE)0) {
             return false;
        }
    }
    return true;
}


bool BitSet::is_intersect(BitSet const& bs) const
{
    ASSERT0(this != &bs);
    BSIdx const first_bit = get_first();
    if (first_bit == BS_UNDEF) {
        return false;
    }
    BSIdx const bs_first_bit = bs.get_first();
    if (bs_first_bit == BS_UNDEF) { //Empty set
        return false;
    }

    BSIdx const start_bit = MAX(first_bit, bs_first_bit);
    BSIdx const end_bit = MIN(get_last(), bs.get_last());
    UINT const start_byte = DIVBPB(start_bit);
    UINT const end_byte = DIVBPB(end_bit);

    for (UINT i = start_byte; i <= end_byte; i++) {
        if ((m_ptr[i] & bs.m_ptr[i]) != (BYTE)0) {
            return true;
        }
    }
    return false;
}


//Return true if 'this' contained in range between 'low' and 'high'.
//'strict': 'this' strictly contained in range.
bool BitSet::is_contained_in_range(BSIdx low, BSIdx high, bool strict) const
{
    ASSERTN(low <= high, ("Invalid bit set"));
    BSIdx const set_low = get_first();
    if (set_low == BS_UNDEF) {
        return false;
    }
    BSIdx const set_high = get_last();
    //In case:
    // low                           high
    // |---------------------------|     given range
    //     set_low        set_high
    //        |--------------|           given bitset
    if (strict) {
        if (set_low > low && set_high < high) {
            return true;
        }
    } else {
        if (set_low >= low && set_high <= high) {
            return true;
        }
    }
    return false;
}


//Return true if 'this' contained range between 'low' and 'high'.
bool BitSet::is_contain_range(BSIdx low, BSIdx high, bool strict) const
{
    ASSERTN(low <= high, ("Invalid bit set"));
    BSIdx const set_low = get_first();
    if (set_low == BS_UNDEF) {
        return false;
    }
    BSIdx const set_high = get_last();
    //In case:
    //         low              high
    //          |--------------|            //given range
    // set_low                     set_high
    //    |---------------------------|        //given set
    if (strict) {
        if (set_low < low && set_high > high) {
            return true;
        }
    } else {
        if (set_low <= low && set_high >= high) {
            return true;
        }
    }
    return false;
}


//Return true if range between first_bit of 'this' and
//last_bit of 'this' overlapped with the range between
//'low' and 'high'.
bool BitSet::is_overlapped(BSIdx low, BSIdx high) const
{
    ASSERTN(low <= high, ("Invalid bit set"));
    BSIdx const set_low = get_first();
    if (set_low == BS_UNDEF) {
        return false;
    }
    BSIdx const set_high = get_last();

    //In case:
    // low                           high
    // |---------------------------|     given range
    //     set_low        set_high
    //        |--------------|           given bitset
    if (set_low >= low && set_high <= high) {
        return true;
    }

    //In case:
    //         low              high
    //          |--------------|
    // set_low                     set_high
    //    |---------------------------|
    if (set_low <= low && set_high >= high) {
        return true;
    }

    //In case:
    //         low              high
    //          |--------------|
    // set_low        set_high
    //    |--------------|
    if (set_high >= low && set_high <= high) {
        return true;
    }

    //In case:
    //       low            high
    //        |--------------|
    //            set_low          set_high
    //               |----------------|
    if (set_low >= low && set_low <= high) {
        return true;
    }
    return false;
}


//Return true if there is elements in the range between 'low' and 'high'.
bool BitSet::has_elem_in_range(BSIdx low, BSIdx high) const
{
    ASSERTN(low <= high, ("out of boundary"));
    BSIdx const first_bit = get_first();
    if (first_bit == BS_UNDEF || first_bit > high) {
        return false;
    }
    if (first_bit >= low && first_bit <= high) {
        return true;
    }
    BSIdx const last_bit = get_last();
    if (last_bit < low) {
        return false;
    }
    BSIdx const start_bit = low;
    BSIdx const end_bit = MIN(last_bit, high);
    if (is_contain(start_bit)) {
        return true;
    }
    if (get_next(start_bit) <= end_bit) {
        return true;
    }
    return false;
}


//Return position of first element, start from '0'.
//Return BS_UNDEF if the bitset is empty.
BSIdx BitSet::get_first() const
{
    if (m_size == 0) { return BS_UNDEF; }
    UINT i = 0;
    UINT m = m_size / BYTES_PER_UNIT * BYTES_PER_UNIT;
    for (BSUNIT const* uint_ptr = (BSUNIT const*)m_ptr;
         i < m; uint_ptr++, i += BYTES_PER_UNIT) {
        if (*uint_ptr == 0) { continue; }
        for (BYTE const* bptr = ((BYTE*)(uint_ptr));
             i < m; bptr++, i++) {
            BYTE byte = *bptr;
            if (byte != (BYTE)0) {
                return g_first_one[byte] + (MULBPB(i));
            }
        }
        ASSERTN(0, ("not arrival"));
    }
    ASSERT0(i <= m_size);
    BSIdx x = ByteOp::get_first_idx(&m_ptr[i], m_size - i);
    return x == BS_UNDEF ? BS_UNDEF : x + (MULBPB(i));
}


//Get bit postition of the last element ONE.
//Return BS_UNDEF if bitset is empty.
BSIdx BitSet::get_last() const
{
    if (m_size == 0) { return BS_UNDEF; }
    UINT m = m_size % BYTES_PER_UNIT;
    UINT i = m_size - 1;
    if (m != 0) {
        for (UINT j = 0; j < m; j++) {
            BYTE byte = m_ptr[i - j];
            if (byte != (BYTE)0) {
                return g_last_one[byte] + (MULBPB(i - j));
            }
        }
        if (m_size <= BYTES_PER_UNIT) {
            return BS_UNDEF;
        }
        i -= m;
    }

    BSUNIT const* unit_ptr;
    UINT j = 0;
    for (unit_ptr = ((BSUNIT const*)(m_ptr)) + i / BYTES_PER_UNIT;
         j <= i; unit_ptr--, j += BYTES_PER_UNIT) {
        if (*unit_ptr == 0) { continue; }
        INT k = BYTES_PER_UNIT - 1;
        for (BYTE const* bptr = ((BYTE*)(unit_ptr)) + k; k >= 0; bptr--, k--) {
            BYTE byte = *bptr;
            if (byte != (BYTE)0) {
                return g_last_one[byte] +
                       (MULBPB(i - j - (BYTES_PER_UNIT - 1 - k)));
            }
        }
    }
    ASSERT0(m_ptr == ((BYTE*)unit_ptr) + sizeof(UINT));
    return BS_UNDEF;
}


//Extract subset in range between 'low' and 'high'.
BitSet * BitSet::get_subset_in_range(BSIdx low, BSIdx high, OUT BitSet & subset)
{
    ASSERTN(low <= high, ("Invalid bit set"));
    ASSERTN(&subset != this, ("overlapped!"));

    subset.clean();
    BSIdx first_bit = get_first();
    if (first_bit == BS_UNDEF) {
        return &subset;
    }
    BSIdx last_bit = get_last();
    if ((low > last_bit) || (high < first_bit)) {
        return &subset;
    }

    UINT const sb_last_byte = DIVBPB(high); //last byte of subset
    UINT const sb_first_byte = DIVBPB(low); //first byte of subset
    UINT const last_byte = DIVBPB(last_bit); //last byte of 'this'
    UINT const first_byte = DIVBPB(first_bit); //first byte of 'this'
    if ((sb_last_byte + 1) > subset.m_size) {
        subset.alloc(sb_last_byte + 1);
    }

    //START and END byte-pos of duplication and which apart from
    //the first and last byte of the extracted range.
    UINT start, end;
    if (first_byte > sb_first_byte) {
        //'this':             first_byte, ...
        //subset: first_byte, ...
        start = first_byte;
        if (last_byte < sb_last_byte) {
            //'this': last_byte,
            //subset:       ,...,last_byte
            end = last_byte;
            ::memcpy(subset.m_ptr + start, m_ptr + start, end - start + 1);
        } else {
            //'this':         ,...,last_byte
            //subset: last_byte,
            //or:
            //'this': last_byte,
            //subset: last_byte,
            end = sb_last_byte - 1;
            if ((INT)end >= (INT)start) {
                //Copy the content of 'this', except for the first and last
                //byte.
                ::memcpy(subset.m_ptr + start, m_ptr + start, end - start + 1);
            }
            BYTE byte = m_ptr[sb_last_byte];
            UINT ofst = MODBPB(high);
            ofst = BITS_PER_BYTE - ofst - 1;
            byte = (BYTE)(byte << ofst);
            byte = (BYTE)(byte >> ofst);
            subset.m_ptr[sb_last_byte] = byte;
        }
        return &subset;
    }

    //'this': first_byte, ...,
    //subset:             first_byte, ...
    //or:
    //'this': first_byte, ...
    //subset: first_byte, ...
    BYTE byte = m_ptr[sb_first_byte];
    UINT ofst = MODBPB(low);
    byte = (BYTE)(byte >> ofst);
    byte = (BYTE)(byte << ofst);
    subset.m_ptr[sb_first_byte] = byte;

    start = sb_first_byte + 1;
    if (last_byte < sb_last_byte) {
        //'this': last_byte,
        //subset:          ,...,last_byte
        end = last_byte;
        ::memcpy(subset.m_ptr + start, m_ptr + start, end - start + 1);
    } else {
        //'this':          ,...,last_byte
        //subset: last_byte,
        //or:
        //'this': last_byte,
        //subset: last_byte,
        end = sb_last_byte - 1;
        BYTE byte2 = 0;
        if ((INT)end >= (INT)start) {
            //Copy the content of 'this', except for the first and last
            //byte.
            ::memcpy(subset.m_ptr + start, m_ptr + start, end - start + 1);
            byte2 = m_ptr[sb_last_byte];
        } else {
            //There are two cases:
            //CASE1: Both subset's first and last byte are the same one.
            //    'this': first/last_byte,
            //    subset: first/last_byte,
            //CASE2: first and last byte are sibling.
            //    'this': first_byte, last_byte,
            //    subset: first_byte, last_byte,
            if (sb_first_byte == sb_last_byte) {
                byte2 = subset.m_ptr[sb_last_byte];
            } else if (sb_first_byte + 1 == sb_last_byte) {
                byte2 = m_ptr[sb_last_byte];
            } else {
                UNREACHABLE();
            }
        }
        UINT ofst2 = MODBPB(high);
        ofst2 = BITS_PER_BYTE - ofst2 - 1;
        byte2 = (BYTE)(byte2 << ofst2);
        byte2 = (BYTE)(byte2 >> ofst2);
        subset.m_ptr[sb_last_byte] = byte2;
    }
    return &subset;
}


//Return BS_UNDEF if it has no other element.
//'elem': return next one to current element.
BSIdx BitSet::get_next(BSIdx elem) const
{
    if (m_size == 0) { return BS_UNDEF; }
    return ByteOp::get_next_idx(m_ptr, m_size, elem);
}


void BitSet::clean()
{
    if (m_ptr == nullptr) { return; }
    ::memset((void*)m_ptr, 0, m_size);
}


//Do copy from 'src' to 'des'.
void BitSet::copy(BitSet const& src)
{
    ASSERTN(this != &src, ("copy self"));
    if (src.m_size == 0) {
        ::memset((void*)m_ptr, 0, m_size);
        return;
    }

    UINT cp_sz = src.m_size; //size needs to copy.
    if (m_size < src.m_size) {
        //src's last byte pos.
        BSIdx l = src.get_last();
        if (l == BS_UNDEF) {
            ::memset((void*)m_ptr, 0, m_size);
            return;
        }

        cp_sz = (UINT)(l / BITS_PER_BYTE) + 1;
        if (m_size < cp_sz) {
            ::free(m_ptr);
            m_ptr = (BYTE*)::malloc(cp_sz);
            m_size = cp_sz;
        } else if (m_size > cp_sz) {
            ::memset((void*)(m_ptr + cp_sz), 0, m_size - cp_sz);
        }
    } else if (m_size > src.m_size) {
        cp_sz = src.m_size;
        ::memset((void*)(m_ptr + cp_sz), 0, m_size - cp_sz);
    }

    ASSERTN(m_ptr != nullptr, ("not yet init"));
    ::memcpy(m_ptr, src.m_ptr, cp_sz);
}


//Support concatenation assignment such as: a=b=c
BitSet const& BitSet::operator = (BitSet const& src)
{
    copy(src);
    return *this;
}


void BitSet::dump(CHAR const* name, bool is_del, UFlag flag,
                  BSIdx last_pos) const
{
    if (name == nullptr) { name = BS_DEF_DUMP_FILE; }
    FO_STATUS ft;
    FileObj fo(name, is_del, false, &ft);
    ASSERTN(ft == FO_SUCC, ("%s create failed!!!", name));
    dump(fo, flag, last_pos);
}


void BitSet::dump(FILE * h, UFlag flag, BSIdx last_pos) const
{
    if (h == nullptr) { return; }
    FileObj fo(h);
    dump(fo, flag, last_pos);
}


void BitSet::dump(FileObj & fo, UFlag flag, BSIdx last_pos) const
{
    ASSERT0(fo.getFileHandler());
    ASSERT0(last_pos == BS_UNDEF || ((UINT)last_pos / BITS_PER_BYTE) < m_size);
    BSIdx elem = get_last();
    if (last_pos != BS_UNDEF) {
        elem = last_pos;
    }
    if (elem == BS_UNDEF) {
        if (flag.have(BS_DUMP_BITSET)) {
            fo.prt("\nbitset:[]");
        }
        if (flag.have(BS_DUMP_POS)) {
            fo.prt("\npos(start at 0):");
        }
        return;
    }
    if (flag.have(BS_DUMP_HEX)) {
        fo.prt("\nbitset(hex):\n\t");
        fo.prt("[0x");
        UINT i = 0;
        for (BYTE byte = m_ptr[i]; i <= DIVBPB(elem); byte = m_ptr[++i]) {
            fo.prt("%x", byte);
        }
        fo.prt("]");
    }

    //Print as binary
    if (flag.have(BS_DUMP_BITSET)) {
        fo.prt("\nbitset:[");
        for (BSIdx i = 0; i <= elem; i++) {
            if (is_contain(i)) {
                fo.prt("1");
            } else {
                fo.prt("0");
            }
        }
        fo.prt("]");
    }

    //Print as position.
    if (flag.have(BS_DUMP_POS)) {
        fo.prt("\npos(start at 0):");
        for (BSIdx j = get_first(); j != BS_UNDEF; j = get_next(j)) {
            fo.prt("%u ", elem);
        }
    }
}
//END BitSet


//
//START BitSetMgr
//
//'h': dump mem usage detail to file.
size_t BitSetMgr::count_mem(FILE * h) const
{
    size_t count = 0;
    C<BitSet*> * ct;
    for (m_bs_list.get_head(&ct);
         ct != m_bs_list.end(); ct = m_bs_list.get_next(ct)) {
        ASSERT0(ct->val());
        count += ct->val()->count_mem();
    }
    DUMMYUSE(h);

    #ifdef _DEBUG_
    if (h != nullptr) {
        //Dump mem usage into file.
        List<size_t> lst;
        C<BitSet*> * ct2;
        for (m_bs_list.get_head(&ct2);
             ct2 != m_bs_list.end(); ct2 = m_bs_list.get_next(ct2)) {
            BitSet const* bs = ct2->val();
            size_t c = bs->count_mem();

            C<size_t> * ct3 = nullptr;
            UINT n = lst.get_elem_count();
            lst.get_head(&ct3);
            UINT i;
            for (i = 0; i < n; i++, ct3 = lst.get_next(ct3)) {
                if (c >= ct3->val()) {
                    lst.insert_before(c, ct3);
                    break;
                }
            }
            if (i == n) {
                lst.append_head(c);
            }
        }

        size_t v = lst.get_head();
        fprintf(h, "\n== DUMP BitSetMgr: total %d bitsets, mem usage are:\n",
                   m_bs_list.get_elem_count());

        UINT b = 0;
        UINT n = lst.get_elem_count();
        for (UINT i = 0; i < n; i++, v = lst.get_next(), b++) {
            if (b == 20) {
                fprintf(h, "\n");
                b = 0;
            }
            if (v < 1024) {
                fprintf(h, "%uB,", (UINT)v);
            } else if (v < 1024 * 1024) {
                fprintf(h, "%uKB,", (UINT)v/1024);
            } else {
                fprintf(h, "%uMB,", (UINT)v/1024/1024);
            }
        }
        fflush(h);
    }
    #endif
    return count;
}
//END BitSetMgr



//
//Binary Operations
//
//Returns a new set which is the union of set1 and set2,
//and modify 'res' as result.
BitSet * bs_union(BitSet const& set1, BitSet const& set2, OUT BitSet & res)
{
    ASSERTN(set1.m_ptr != nullptr && set2.m_ptr != nullptr &&
            res.m_ptr != nullptr, ("not yet init"));
    if (&res == &set1) {
        res.bunion(set2);
    } else if (&res == &set2) {
        res.bunion(set1);
    } else if (set1.m_size < set2.m_size) {
        res.copy(set2);
        res.bunion(set1);
    } else {
        res.copy(set1);
        res.bunion(set2);
    }
    return &res;
}


//Subtracting set2 from set1
//Returns a new set which is:
//{ x : member( x, 'set1' ) & ~ member( x, 'set2' ) }.
BitSet * bs_diff(BitSet const& set1, BitSet const& set2, OUT BitSet & res)
{
    ASSERTN(set1.m_ptr != nullptr && set2.m_ptr != nullptr &&
            res.m_ptr != nullptr, ("not yet init"));
    if (&res == &set1) {
        res.diff(set2);
    } else if (&res == &set2) {
        BitSet tmp(set1);
        tmp.diff(set2);
        res.copy(tmp);
    } else {
        res.copy(set1);
        res.diff(set2);
    }
    return &res;
}


//Returns a new set which is intersection of 'set1' and 'set2'.
BitSet * bs_intersect(BitSet const& set1, BitSet const& set2, OUT BitSet & res)
{
    ASSERTN(set1.m_ptr != nullptr && set2.m_ptr != nullptr &&
            res.m_ptr != nullptr, ("not yet init"));
    if (&res == &set1) {
        res.intersect(set2);
    } else if (&res == &set2) {
        res.intersect(set1);
    } else {
        res.copy(set1);
        res.intersect(set2);
    }
    return &res;
}

} //namespace xcom
