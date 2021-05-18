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

#if BITS_PER_BYTE == 8
#define DIVBPB(a) ((a) >> 3)
#define MULBPB(a) ((a) << 3)
#define MODBPB(a) ((a) & 7)
#else
#define DIVBPB(a) ((a) / BITS_PER_BYTE)
#define MULBPB(a) ((a) * BITS_PER_BYTE)
#define MODBPB(a) ((a) % BITS_PER_BYTE)
#endif

//Count of bits in all the one byte numbers.
BYTE const g_bit_count[256] = {
  0, //  0
  1, //  1
  1, //  2
  2, //  3
  1, //  4
  2, //  5
  2, //  6
  3, //  7
  1, //  8
  2, //  9
  2, // 10
  3, // 11
  2, // 12
  3, // 13
  3, // 14
  4, // 15
  1, // 16
  2, // 17
  2, // 18
  3, // 19
  2, // 20
  3, // 21
  3, // 22
  4, // 23
  2, // 24
  3, // 25
  3, // 26
  4, // 27
  3, // 28
  4, // 29
  4, // 30
  5, // 31
  1, // 32
  2, // 33
  2, // 34
  3, // 35
  2, // 36
  3, // 37
  3, // 38
  4, // 39
  2, // 40
  3, // 41
  3, // 42
  4, // 43
  3, // 44
  4, // 45
  4, // 46
  5, // 47
  2, // 48
  3, // 49
  3, // 50
  4, // 51
  3, // 52
  4, // 53
  4, // 54
  5, // 55
  3, // 56
  4, // 57
  4, // 58
  5, // 59
  4, // 60
  5, // 61
  5, // 62
  6, // 63
  1, // 64
  2, // 65
  2, // 66
  3, // 67
  2, // 68
  3, // 69
  3, // 70
  4, // 71
  2, // 72
  3, // 73
  3, // 74
  4, // 75
  3, // 76
  4, // 77
  4, // 78
  5, // 79
  2, // 80
  3, // 81
  3, // 82
  4, // 83
  3, // 84
  4, // 85
  4, // 86
  5, // 87
  3, // 88
  4, // 89
  4, // 90
  5, // 91
  4, // 92
  5, // 93
  5, // 94
  6, // 95
  2, // 96
  3, // 97
  3, // 98
  4, // 99
  3, //100
  4, //101
  4, //102
  5, //103
  3, //104
  4, //105
  4, //106
  5, //107
  4, //108
  5, //109
  5, //110
  6, //111
  3, //112
  4, //113
  4, //114
  5, //115
  4, //116
  5, //117
  5, //118
  6, //119
  4, //120
  5, //121
  5, //122
  6, //123
  5, //124
  6, //125
  6, //126
  7, //127
  1, //128
  2, //129
  2, //130
  3, //131
  2, //132
  3, //133
  3, //134
  4, //135
  2, //136
  3, //137
  3, //138
  4, //139
  3, //140
  4, //141
  4, //142
  5, //143
  2, //144
  3, //145
  3, //146
  4, //147
  3, //148
  4, //149
  4, //150
  5, //151
  3, //152
  4, //153
  4, //154
  5, //155
  4, //156
  5, //157
  5, //158
  6, //159
  2, //160
  3, //161
  3, //162
  4, //163
  3, //164
  4, //165
  4, //166
  5, //167
  3, //168
  4, //169
  4, //170
  5, //171
  4, //172
  5, //173
  5, //174
  6, //175
  3, //176
  4, //177
  4, //178
  5, //179
  4, //180
  5, //181
  5, //182
  6, //183
  4, //184
  5, //185
  5, //186
  6, //187
  5, //188
  6, //189
  6, //190
  7, //191
  2, //192
  3, //193
  3, //194
  4, //195
  3, //196
  4, //197
  4, //198
  5, //199
  3, //200
  4, //201
  4, //202
  5, //203
  4, //204
  5, //205
  5, //206
  6, //207
  3, //208
  4, //209
  4, //210
  5, //211
  4, //212
  5, //213
  5, //214
  6, //215
  4, //216
  5, //217
  5, //218
  6, //219
  5, //220
  6, //221
  6, //222
  7, //223
  3, //224
  4, //225
  4, //226
  5, //227
  4, //228
  5, //229
  5, //230
  6, //231
  4, //232
  5, //233
  5, //234
  6, //235
  5, //236
  6, //237
  6, //238
  7, //239
  4, //240
  5, //241
  5, //242
  6, //243
  5, //244
  6, //245
  6, //246
  7, //247
  5, //248
  6, //249
  6, //250
  7, //251
  6, //252
  7, //253
  7, //254
  8  //255
};


//Mapping from 8 bit unsigned integers to the index of the first one bit.
static BYTE const g_first_one[256] = {
  0, //   no one
  0, //   1 the first bit in 0th pos
  1, //   2 the first bit in 1th pos
  0, //   3 the first bit in 0th pos
  2, //   4 the first bit in 2th pos
  0, //   5
  1, //   6
  0, //   7
  3, //   8
  0, //   9
  1, //  10
  0, //  11
  2, //  12
  0, //  13
  1, //  14
  0, //  15
  4, //  16
  0, //  17
  1, //  18
  0, //  19
  2, //  20
  0, //  21
  1, //  22
  0, //  23
  3, //  24
  0, //  25
  1, //  26
  0, //  27
  2, //  28
  0, //  29
  1, //  30
  0, //  31
  5, //  32
  0, //  33
  1, //  34
  0, //  35
  2, //  36
  0, //  37
  1, //  38
  0, //  39
  3, //  40
  0, //  41
  1, //  42
  0, //  43
  2, //  44
  0, //  45
  1, //  46
  0, //  47
  4, //  48
  0, //  49
  1, //  50
  0, //  51
  2, //  52
  0, //  53
  1, //  54
  0, //  55
  3, //  56
  0, //  57
  1, //  58
  0, //  59
  2, //  60
  0, //  61
  1, //  62
  0, //  63
  6, //  64
  0, //  65
  1, //  66
  0, //  67
  2, //  68
  0, //  69
  1, //  70
  0, //  71
  3, //  72
  0, //  73
  1, //  74
  0, //  75
  2, //  76
  0, //  77
  1, //  78
  0, //  79
  4, //  80
  0, //  81
  1, //  82
  0, //  83
  2, //  84
  0, //  85
  1, //  86
  0, //  87
  3, //  88
  0, //  89
  1, //  90
  0, //  91
  2, //  92
  0, //  93
  1, //  94
  0, //  95
  5, //  96
  0, //  97
  1, //  98
  0, //  99
  2, // 100
  0, // 101
  1, // 102
  0, // 103
  3, // 104
  0, // 105
  1, // 106
  0, // 107
  2, // 108
  0, // 109
  1, // 110
  0, // 111
  4, // 112
  0, // 113
  1, // 114
  0, // 115
  2, // 116
  0, // 117
  1, // 118
  0, // 119
  3, // 120
  0, // 121
  1, // 122
  0, // 123
  2, // 124
  0, // 125
  1, // 126
  0, // 127
  7, // 128
  0, // 129
  1, // 130
  0, // 131
  2, // 132
  0, // 133
  1, // 134
  0, // 135
  3, // 136
  0, // 137
  1, // 138
  0, // 139
  2, // 140
  0, // 141
  1, // 142
  0, // 143
  4, // 144
  0, // 145
  1, // 146
  0, // 147
  2, // 148
  0, // 149
  1, // 150
  0, // 151
  3, // 152
  0, // 153
  1, // 154
  0, // 155
  2, // 156
  0, // 157
  1, // 158
  0, // 159
  5, // 160
  0, // 161
  1, // 162
  0, // 163
  2, // 164
  0, // 165
  1, // 166
  0, // 167
  3, // 168
  0, // 169
  1, // 170
  0, // 171
  2, // 172
  0, // 173
  1, // 174
  0, // 175
  4, // 176
  0, // 177
  1, // 178
  0, // 179
  2, // 180
  0, // 181
  1, // 182
  0, // 183
  3, // 184
  0, // 185
  1, // 186
  0, // 187
  2, // 188
  0, // 189
  1, // 190
  0, // 191
  6, // 192
  0, // 193
  1, // 194
  0, // 195
  2, // 196
  0, // 197
  1, // 198
  0, // 199
  3, // 200
  0, // 201
  1, // 202
  0, // 203
  2, // 204
  0, // 205
  1, // 206
  0, // 207
  4, // 208
  0, // 209
  1, // 210
  0, // 211
  2, // 212
  0, // 213
  1, // 214
  0, // 215
  3, // 216
  0, // 217
  1, // 218
  0, // 219
  2, // 220
  0, // 221
  1, // 222
  0, // 223
  5, // 224
  0, // 225
  1, // 226
  0, // 227
  2, // 228
  0, // 229
  1, // 230
  0, // 231
  3, // 232
  0, // 233
  1, // 234
  0, // 235
  2, // 236
  0, // 237
  1, // 238
  0, // 239
  4, // 240
  0, // 241
  1, // 242
  0, // 243
  2, // 244
  0, // 245
  1, // 246
  0, // 247
  3, // 248
  0, // 249
  1, // 250
  0, // 251
  2, // 252
  0, // 253
  1, // 254
  0, // 255
};


static BYTE const g_last_one[256] = {
  0, //  0
  0, //  1
  1, //  2
  1, //  3
  2, //  4
  2, //  5
  2, //  6
  2, //  7
  3, //  8
  3, //  9
  3, // 10
  3, // 11
  3, // 12
  3, // 13
  3, // 14
  3, // 15
  4, // 16
  4, // 17
  4, // 18
  4, // 19
  4, // 20
  4, // 21
  4, // 22
  4, // 23
  4, // 24
  4, // 25
  4, // 26
  4, // 27
  4, // 28
  4, // 29
  4, // 30
  4, // 31
  5, // 32
  5, // 33
  5, // 34
  5, // 35
  5, // 36
  5, // 37
  5, // 38
  5, // 39
  5, // 40
  5, // 41
  5, // 42
  5, // 43
  5, // 44
  5, // 45
  5, // 46
  5, // 47
  5, // 48
  5, // 49
  5, // 50
  5, // 51
  5, // 52
  5, // 53
  5, // 54
  5, // 55
  5, // 56
  5, // 57
  5, // 58
  5, // 59
  5, // 60
  5, // 61
  5, // 62
  5, // 63
  6, // 64
  6, // 65
  6, // 66
  6, // 67
  6, // 68
  6, // 69
  6, // 70
  6, // 71
  6, // 72
  6, // 73
  6, // 74
  6, // 75
  6, // 76
  6, // 77
  6, // 78
  6, // 79
  6, // 80
  6, // 81
  6, // 82
  6, // 83
  6, // 84
  6, // 85
  6, // 86
  6, // 87
  6, // 88
  6, // 89
  6, // 90
  6, // 91
  6, // 92
  6, // 93
  6, // 94
  6, // 95
  6, // 96
  6, // 97
  6, // 98
  6, // 99
  6, //100
  6, //101
  6, //102
  6, //103
  6, //104
  6, //105
  6, //106
  6, //107
  6, //108
  6, //109
  6, //110
  6, //111
  6, //112
  6, //113
  6, //114
  6, //115
  6, //116
  6, //117
  6, //118
  6, //119
  6, //120
  6, //121
  6, //122
  6, //123
  6, //124
  6, //125
  6, //126
  6, //127
  7, //128
  7, //129
  7, //130
  7, //131
  7, //132
  7, //133
  7, //134
  7, //135
  7, //136
  7, //137
  7, //138
  7, //139
  7, //140
  7, //141
  7, //142
  7, //143
  7, //144
  7, //145
  7, //146
  7, //147
  7, //148
  7, //149
  7, //150
  7, //151
  7, //152
  7, //153
  7, //154
  7, //155
  7, //156
  7, //157
  7, //158
  7, //159
  7, //160
  7, //161
  7, //162
  7, //163
  7, //164
  7, //165
  7, //166
  7, //167
  7, //168
  7, //169
  7, //170
  7, //171
  7, //172
  7, //173
  7, //174
  7, //175
  7, //176
  7, //177
  7, //178
  7, //179
  7, //180
  7, //181
  7, //182
  7, //183
  7, //184
  7, //185
  7, //186
  7, //187
  7, //188
  7, //189
  7, //190
  7, //191
  7, //192
  7, //193
  7, //194
  7, //195
  7, //196
  7, //197
  7, //198
  7, //199
  7, //200
  7, //201
  7, //202
  7, //203
  7, //204
  7, //205
  7, //206
  7, //207
  7, //208
  7, //209
  7, //210
  7, //211
  7, //212
  7, //213
  7, //214
  7, //215
  7, //216
  7, //217
  7, //218
  7, //219
  7, //220
  7, //221
  7, //222
  7, //223
  7, //224
  7, //225
  7, //226
  7, //227
  7, //228
  7, //229
  7, //230
  7, //231
  7, //232
  7, //233
  7, //234
  7, //235
  7, //236
  7, //237
  7, //238
  7, //239
  7, //240
  7, //241
  7, //242
  7, //243
  7, //244
  7, //245
  7, //246
  7, //247
  7, //248
  7, //249
  7, //250
  7, //251
  7, //252
  7, //253
  7, //254
  7, //255
};


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
        ::memset(((BYTE*)p) + orgsize, 0, newsize - orgsize);
    } else {
        ::memset(p, 0, newsize);
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
        ::memset(m_ptr, 0, m_size);
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
        INT l = bs.get_last();
        if (l < 0) { return; }
        cp_sz = l / BITS_PER_BYTE + 1;
        if (m_size < cp_sz) {
            m_ptr = (BYTE*)realloc(m_ptr, m_size, cp_sz);
            m_size = cp_sz;
        }
    }
    UINT const num_of_uint = cp_sz / BYTES_PER_UINT; //floor-div.
    ASSERTN(m_ptr, ("not yet init"));
    UINT * uint_ptr_this = (UINT*)m_ptr;
    UINT * uint_ptr_bs = (UINT*)bs.m_ptr;
    for (UINT i = 0; i < num_of_uint; i++) {
        uint_ptr_this[i] |= uint_ptr_bs[i];
    }
    for (UINT i = num_of_uint * BYTES_PER_UINT; i < cp_sz; i++) {
        m_ptr[i] |= bs.m_ptr[i];
    }
}


//Add a element which corresponding to 'elem' bit, and set this bit.
void BitSet::bunion(UINT elem)
{
    UINT const first_byte = DIVBPB(elem);
    if (m_size < (first_byte+1)) {
        m_ptr = (BYTE*)realloc(m_ptr, m_size, first_byte + 1);
        m_size = first_byte+1;
    }
    elem = MODBPB(elem);
    m_ptr[first_byte] |= (BYTE)(1 << elem);
}


//The difference operation calculates the elements that
//distinguish one set from another.
//Remove a element which map with 'elem' bit, and clean this bit.
void BitSet::diff(UINT elem)
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
    UINT num_of_uint = minsize / BYTES_PER_UINT;
    UINT * uint_ptr_this = (UINT*)m_ptr;
    UINT * uint_ptr_bs = (UINT*)bs.m_ptr;
    for (UINT i = 0; i < num_of_uint; i++) {
        UINT d = uint_ptr_bs[i];
        if (d != 0) {
            uint_ptr_this[i] &= ~d;
        }
    }

    ASSERTN(m_ptr != nullptr, ("not yet init"));
    for (UINT i = num_of_uint * BYTES_PER_UINT; i < minsize; i++) {
        BYTE d = bs.m_ptr[i];
        if (d != 0) {
            m_ptr[i] &= ~d;
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
        ::memset(m_ptr + bs.m_size, 0, m_size - bs.m_size);
    } else {
        for (UINT i = 0; i < m_size; i++) {
            m_ptr[i] &= bs.m_ptr[i];
        }
    }
}


//Reverse each bit.
//e.g: 1001 to 0110
//'last_bit_pos': start at 0, e.g:given '101', last bit pos is 2.
void BitSet::rev(UINT last_bit_pos)
{
    ASSERTN(m_ptr != nullptr, ("can not reverse empty set"));
    UINT const last_byte_pos = last_bit_pos / BITS_PER_BYTE;
    ASSERT0(last_byte_pos < m_size);

    UINT const n = last_byte_pos / BYTES_PER_UINT;
    for (UINT i = 0; i < n; i++) {
        ((UINT*)m_ptr)[i] = ~(((UINT*)m_ptr)[i]);
    }

    for (UINT i = n * BYTES_PER_UINT; i < last_byte_pos; i++) {
        m_ptr[i] = ~m_ptr[i];
    }

    //Here we use UINT type to avoid byte truncate operation.
    UINT last_byte = m_ptr[last_byte_pos];

    UINT const mask = (1 << (last_bit_pos % BITS_PER_BYTE + 1)) - 1;
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
    ASSERT0(BYTES_PER_UINT == 4);
    UINT const m = m_size / BYTES_PER_UINT;
    for (UINT i = 0; i < m; i++) {
        INT v = ((INT*)(m_ptr))[i];
        v = v - ((v >> 1) & 0x55555555);
        v = (v & 0x33333333) + ((v >> 2) & 0x33333333);
        count += (((v + (v >> 4)) & 0x0F0F0F0F) * 0x01010101) >> 24;
    }
    for (UINT i = m * BYTES_PER_UINT; i < m_size; i++) {
        count += g_bit_count[m_ptr[i]];
    }
#else
    for (UINT i = 0; i < m_size; i++) {
        count += g_bit_count[m_ptr[i]];
    }
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

    UINT v1 = *(UINT*)ptr1;
    UINT v2 = *(UINT*)ptr2;
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
bool BitSet::is_contain(UINT elem) const
{
    if (m_ptr == nullptr) { return false; }
    if (elem >= (MULBPB(m_size))) {
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
    INT const first_bit = get_first();
    if (first_bit == -1) {
        return false;
    }
    INT const bs_first_bit = bs.get_first();
    if (bs_first_bit == -1) {
        //nullptr set be contained for any set.
        return true;
    }

    if (first_bit > bs_first_bit) {
        return false;
    } else if (strict && first_bit < bs_first_bit) {
        scon = true;
    }

    INT const start_bit = MAX(first_bit, bs_first_bit);
    INT const start_byte = DIVBPB(start_bit);

    if (m_size < bs.m_size) {//'this' is smaller
        if (bs.get_next(MULBPB(m_size) - 1) != -1) {
            //'bs' has more elements than 'this'.
            return false;
        }
    } else if (strict && m_size > bs.m_size) {//'bs' is smaller
        if (get_next(MULBPB(bs.m_size) - 1) != -1) {
            //'this' has more elements than 'this'.
            scon = true;
        }
    }

    INT const minsize = MIN(m_size, bs.m_size);
    for (INT i = start_byte; i < minsize; i++) {
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
    UINT num_of_uint = m_size / BYTES_PER_UINT;
    UINT * uint_ptr = (UINT*)m_ptr;
    for (UINT i = 0; i < num_of_uint; i++) {
        if (uint_ptr[i] != 0) {
             return false;
        }
    }
    for (UINT i = num_of_uint * BYTES_PER_UINT; i < m_size; i++) {
        if (m_ptr[i] != (BYTE)0) {
             return false;
        }
    }
    return true;
}


bool BitSet::is_intersect(BitSet const& bs) const
{
    ASSERT0(this != &bs);
    INT const first_bit = get_first();
    if (first_bit == -1) {
        return false;
    }
    INT const bs_first_bit = bs.get_first();
    if (bs_first_bit == -1) { //Empty set
        return false;
    }

    UINT const start_bit = MAX(first_bit, bs_first_bit);
    UINT const end_bit = MIN(get_last(), bs.get_last());
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
bool BitSet::is_contained_in_range(UINT low, UINT high, bool strict) const
{
    ASSERTN(low <= high, ("Invalid bit set"));
    INT const set_low = get_first();
    if (set_low == -1) {
        return false;
    }
    INT const set_high = get_last();
    //In case:
    // low                           high
    // |---------------------------|     given range
    //     set_low        set_high
    //        |--------------|           given bitset
    if (strict) {
        if ((UINT)set_low > low && (UINT)set_high < high) {
            return true;
        }
    } else {
        if ((UINT)set_low >= low && (UINT)set_high <= high) {
            return true;
        }
    }
    return false;
}


//Return true if 'this' contained range between 'low' and 'high'.
bool BitSet::is_contain_range(UINT low, UINT high, bool strict) const
{
    ASSERTN(low <= high, ("Invalid bit set"));
    INT const set_low = get_first();
    if (set_low == -1) {
        return false;
    }
    INT const set_high = get_last();
    //In case:
    //         low              high
    //          |--------------|            //given range
    // set_low                     set_high
    //    |---------------------------|        //given set
    if (strict) {
        if ((UINT)set_low < low && (UINT)set_high > high) {
            return true;
        }
    } else {
        if ((UINT)set_low <= low && (UINT)set_high >= high) {
            return true;
        }
    }
    return false;
}


//Return true if range between first_bit of 'this' and
//last_bit of 'this' overlapped with the range between
//'low' and 'high'.
bool BitSet::is_overlapped(UINT low, UINT high) const
{
    ASSERTN(low <= high, ("Invalid bit set"));
    INT const set_low = get_first();
    if (set_low == -1) {
        return false;
    }
    INT const set_high = get_last();

    //In case:
    // low                           high
    // |---------------------------|     given range
    //     set_low        set_high
    //        |--------------|           given bitset
    if ((UINT)set_low >= low && (UINT)set_high <= high) {
        return true;
    }

    //In case:
    //         low              high
    //          |--------------|
    // set_low                     set_high
    //    |---------------------------|
    if ((UINT)set_low <= low && (UINT)set_high >= high) {
        return true;
    }

    //In case:
    //         low              high
    //          |--------------|
    // set_low        set_high
    //    |--------------|
    if ((UINT)set_high >= low && (UINT)set_high <= high) {
        return true;
    }

    //In case:
    //       low            high
    //        |--------------|
    //            set_low          set_high
    //               |----------------|
    if ((UINT)set_low >= low && (UINT)set_low <= high) {
        return true;
    }
    return false;
}


//Return true if there is elements in the range between 'low' and 'high'.
bool BitSet::has_elem_in_range(UINT low, UINT high) const
{
    ASSERTN(low <= high, ("out of boundary"));
    INT const first_bit = get_first();
    if (first_bit == -1 || (UINT)first_bit > high) {
        return false;
    }
    if ((UINT)first_bit >= low && (UINT)first_bit <= high) {
        return true;
    }
    INT const last_bit = get_last();
    if ((UINT)last_bit < low) {
        return false;
    }
    INT const start_bit = low;
    INT const end_bit = MIN((UINT)last_bit, high);
    if (is_contain(start_bit)) {
        return true;
    }
    if (get_next(start_bit) <= end_bit) {
        return true;
    }
    return false;
}


//Return position of first element, start from '0'.
//Return -1 if the bitset is empty.
INT BitSet::get_first() const
{
    if (m_size == 0) { return -1; }
    UINT i = 0;
    UINT m = m_size / BYTES_PER_UINT * BYTES_PER_UINT;
    for (UINT const* uint_ptr = ((UINT const*)(m_ptr));
         i < m; uint_ptr++, i += 4) {
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
    for (; i < m_size; i++) {
        BYTE byte = m_ptr[i];
        if (byte != (BYTE)0) {
            return g_first_one[byte] + (MULBPB(i));
        }
    }
    return -1;
}


//Get bit postition of the last element ONE.
//Return -1 if bitset is empty.
INT BitSet::get_last() const
{
    if (m_size == 0) { return -1; }
    INT i = m_size - 1;
    UINT m = m_size % BYTES_PER_UINT;
    if (m == 0) {
        m = BYTES_PER_UINT;
    }
    for (; i >= (INT)(m_size - m); i--) {
        BYTE byte = m_ptr[i];
        if (byte != (BYTE)0) {
            return g_last_one[byte] + (MULBPB(i));
        }
    }
    if (i < 0) { return -1; }
    UINT const* uint_ptr;
    for (uint_ptr = ((UINT const*)(m_ptr)) + i / BYTES_PER_UINT;
         i >= 0; uint_ptr--, i -= 4) {
        if (*uint_ptr == 0) { continue; }
        for (BYTE const* bptr = ((BYTE*)(uint_ptr)) + BYTES_PER_UINT - 1;
             i >= 0; bptr--, i--) {
            BYTE byte = *bptr;
            if (byte != (BYTE)0) {
                return g_last_one[byte] + (MULBPB(i));
            }
        }
    }
    ASSERT0(m_ptr == ((BYTE*)uint_ptr) + sizeof(UINT));
    return -1;
}


//Extract subset in range between 'low' and 'high'.
BitSet * BitSet::get_subset_in_range(UINT low, UINT high, OUT BitSet & subset)
{
    ASSERTN(low <= high, ("Invalid bit set"));
    ASSERTN(&subset != this, ("overlapped!"));

    subset.clean();
    INT first_bit = get_first();
    if (first_bit == -1) {
        return &subset;
    }
    INT last_bit = get_last();
    if ((low > (UINT)last_bit) || (high < (UINT)first_bit)) {
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
    INT start, end;
    if (first_byte > sb_first_byte) {
        //'this':             first_byte, ...
        //subset: first_byte, ...
        start = first_byte;
        if (last_byte < sb_last_byte) {
            //'this': last_byte,
            //subset:       ,...,last_byte
            end = last_byte;
            ::memcpy(subset.m_ptr + start,
                    m_ptr + start, end - start + 1);
        } else {
            //'this':         ,...,last_byte
            //subset: last_byte,
            //or:
            //'this': last_byte,
            //subset: last_byte,
            end = sb_last_byte - 1;
            if (end >= start) {
                //Copy the content of 'this', except for
                //the first and last byte.
                ::memcpy(subset.m_ptr + start,
                        m_ptr + start, end - start + 1);
            }
            BYTE byte = m_ptr[sb_last_byte];
            UINT ofst = MODBPB(high);
            ofst = BITS_PER_BYTE - ofst - 1;
            byte <<= ofst;
            byte >>= ofst;
            subset.m_ptr[sb_last_byte] = byte;
        }
    } else {
        //'this': first_byte, ...,
        //subset:             first_byte, ...
        //or:
        //'this': first_byte, ...
        //subset: first_byte, ...
        BYTE byte = m_ptr[sb_first_byte];
        UINT ofst = MODBPB(low);
        byte >>= ofst;
        byte <<= ofst;
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
            if (end >= start) {
                //Copy the content of 'this', except
                //for the first and last byte.
                ::memcpy(subset.m_ptr + start,
                        m_ptr + start, end - start + 1);
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
            byte2 <<= ofst2;
            byte2 >>= ofst2;
            subset.m_ptr[sb_last_byte] = byte2;
        }
    }
    return &subset;
}


//Return -1 if it has no other element.
//'elem': return next one to current element.
INT BitSet::get_next(UINT elem) const
{
    if (m_size == 0) { return -1; }
    INT first_byte = DIVBPB(elem);
    if (((UINT)first_byte + 1) > m_size) {
        return -1;
    }
    BYTE byte = m_ptr[first_byte];
    elem = MODBPB(elem) + 1; //index in 'first_byte'.
    byte >>= elem; //Erase low 'elem' bits.
    byte <<= elem;
    if (byte == 0) {
        //No elements in this byte.
        UINT i = first_byte + 1;
        while (i < m_size) {
            byte = m_ptr[i];
            if (byte != 0) {
                first_byte = i;
                break;
            }
            i++;
        }
        if (byte == 0) {
            return -1;
        }
    }
    return g_first_one[byte] + (MULBPB(first_byte));
}


void BitSet::clean()
{
    if (m_ptr == nullptr) { return; }
    ::memset(m_ptr, 0, m_size);
}


//Do copy from 'src' to 'des'.
void BitSet::copy(BitSet const& src)
{
    ASSERTN(this != &src, ("copy self"));
    if (src.m_size == 0) {
        ::memset(m_ptr, 0, m_size);
        return;
    }

    UINT cp_sz = src.m_size; //size needs to copy.
    if (m_size < src.m_size) {
        //src's last byte pos.
        INT l = src.get_last();
        if (l < 0) {
            ::memset(m_ptr, 0, m_size);
            return;
        }

        cp_sz = l / BITS_PER_BYTE + 1;
        if (m_size < cp_sz) {
            ::free(m_ptr);
            m_ptr = (BYTE*)::malloc(cp_sz);
            m_size = cp_sz;
        } else if (m_size > cp_sz) {
            ::memset(m_ptr + cp_sz, 0, m_size - cp_sz);
        }
    } else if (m_size > src.m_size) {
        cp_sz = src.m_size;
        ::memset(m_ptr + cp_sz, 0, m_size - cp_sz);
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


void BitSet::dump(CHAR const* name, bool is_del, UINT flag, INT last_pos) const
{
    if (name == nullptr) {
        name = "zbs.cxx";
    }
    if (is_del) {
        UNLINK(name);
    }
    FILE * h = fopen(name, "a+");
    ASSERTN(h != nullptr, ("%s create failed!!!", name));
    dump(h, flag, last_pos);
    fclose(h);
}


void BitSet::dump(FILE * h, UINT flag, INT last_pos) const
{
    if (h == nullptr) { return; }
    ASSERT0(last_pos < 0 || (last_pos / BITS_PER_BYTE) < (INT)m_size);

    INT elem = get_last();
    if (last_pos != -1) {
        elem = last_pos;
    }
    if (elem < 0) {
        if (HAVE_FLAG(flag, BS_DUMP_BITSET)) {
            fprintf(h, "\nbitset:[]");
        }
        if (HAVE_FLAG(flag, BS_DUMP_POS)) {
            fprintf(h, "\npos(start at 0):");
        }
    } else {
        INT i = 0;
        //fprintf(h, "\nbitset(hex):\n\t");
        //fprintf(h, "[0x");
        //for (BYTE byte = m_ptr[i]; i <= DIVBPB(elem); byte = m_ptr[++i]) {
        //    fprintf(h, "%x", byte);
        //}
        //fprintf(h, "]");

        //Print as binary
        if (HAVE_FLAG(flag, BS_DUMP_BITSET)) {
            fprintf(h, "\nbitset:[");
            i = 0;
            do {
                if (is_contain(i)) {
                    fprintf(h, "1");
                } else {
                    fprintf(h, "0");
                }
                i++;
            } while (i <= elem);
            fprintf(h, "]");
        }

        //Print as position
        if (HAVE_FLAG(flag, BS_DUMP_POS)) {
            fprintf(h, "\npos(start at 0):");
            elem = get_first();
            do {
                fprintf(h, "%d ", elem);
                elem = get_next(elem);
            } while (elem >= 0);
        }
    }
    fflush(h);
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
    ASSERTN(set1.m_ptr != nullptr && set2.m_ptr != nullptr && res.m_ptr != nullptr,
            ("not yet init"));
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
//Returns a new set which is { x : member( x, 'set1' ) & ~ member( x, 'set2' ) }.
BitSet * bs_diff(BitSet const& set1, BitSet const& set2, OUT BitSet & res)
{
    ASSERTN(set1.m_ptr != nullptr &&
            set2.m_ptr != nullptr &&
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
    ASSERTN(set1.m_ptr != nullptr &&
            set2.m_ptr != nullptr &&
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
