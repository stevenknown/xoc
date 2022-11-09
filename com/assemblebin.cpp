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

//#define WORD_SIZE_ALIGN

namespace xcom {

AssembleBinBuf::AssembleBinBuf(BYTE * buf, UINT buflen,
                               AssembleBinDescVec const& descvec)
{
    UINT explen;
    ASSERTN(isBufBigEnough(buflen, descvec, &explen),
            ("expect at least buffer length %u bytes", explen));
    m_bytebuf = buf;
    m_bytebuflen = buflen;
    m_cur_bitofst = 0;
    for (VecIdx i = 0; i < (VecIdx)descvec.get_elem_count(); i++) {
        AssembleBinDesc const* desc = descvec.get_elem_addr(i);
        if (desc->isWordValid()) {
            writeWord(desc);
        } else {
            writeBuf(desc);
        }
    }
}


bool AssembleBinBuf::isBufBigEnough(UINT bytebuflen,
                                    AssembleBinDescVec const& descvec,
                                    OUT UINT * expectbuflen)
{
    UINT bitsize = 0;
    for (VecIdx i = 0; i < (VecIdx)descvec.get_elem_count(); i++) {
        AssembleBinDesc const* desc = descvec.get_elem_addr(i);
        bitsize += desc->bitsize;
    }
    UINT expsize = 0;
    #ifdef WORD_SIZE_ALIGN
    expsize = xcom::xceiling(bitsize, BIN_WORD_SIZE) * BYTES_PER_WORD;
    if (expectbuflen != nullptr) {
        *expectbuflen = expsize;
    }
    #else
    //Byte size align
    expsize = xcom::xceiling(bitsize, BITS_PER_BYTE);
    if (expectbuflen != nullptr) {
        *expectbuflen = expsize;
    }
    #endif
    return expsize <= bytebuflen;
}


bool AssembleBinBuf::isImpure(BinWord word, UINT bitsize)
{
    if (bitsize >= BIN_WORD_SIZE) {
        //CASE:bitsize that larger than BIN_WORD_SIZE, may cause cycle logical
        //right shifting, e.g:x86.
        return false;
    }
    word >>= bitsize;
    return word != 0;
}


void AssembleBinBuf::filterImpurity(BinWord & word, UINT bitsize)
{
    ASSERTN(bitsize < BIN_WORD_SIZE, ("no impurity in word"));
    //Using two assignment-stmt rather than one stmt to avoid the
    //implicitly type hoisting from UINT16 to UINT32.
    word = word << (BIN_WORD_SIZE - bitsize);
    word = word >> (BIN_WORD_SIZE - bitsize);
}


static inline void writeEntireWord(BinWord * start, BinWord word)
{
    *start = word;
}


static inline void writePartialWord(BinWord * start, BinWord word,
                                    UINT bytesize)
{
    ASSERT0(bytesize <= BYTES_PER_WORD);
    ::memcpy((BYTE*)start, (BYTE*)&word, bytesize);
}


void AssembleBinBuf::writeWord(BinWord word, UINT bitsize)
{
    UINT start_bit_in_word = m_cur_bitofst % BIN_WORD_SIZE;
    UINT wordofst = m_cur_bitofst / BIN_WORD_SIZE;
    ASSERT0(!isImpure(word, bitsize));
    if (start_bit_in_word == 0) {
        #ifdef WORD_SIZE_ALIGN
        writeEntireWord(((BinWord*)m_bytebuf) + wordofst, word);
        #else
        writePartialWord(((BinWord*)m_bytebuf) + wordofst, word,
                         xceiling(bitsize, BITS_PER_BYTE));
        #endif
        m_cur_bitofst += bitsize;
        return;
    }
    BinWord last_bin_word = *(((BinWord*)m_bytebuf) + wordofst);
    BinWord newval_part1 = word << start_bit_in_word;
    last_bin_word |= newval_part1;
    *(((BinWord*)m_bytebuf) + wordofst) = last_bin_word;
    m_cur_bitofst += bitsize;
    if ((start_bit_in_word + bitsize) <= BIN_WORD_SIZE) {
        return;
    }
    BinWord newval_part2 = word >> (BIN_WORD_SIZE - start_bit_in_word);
    #ifdef WORD_SIZE_ALIGN
    writeEntireWord(((BinWord*)m_bytebuf) + wordofst + 1, newval_part2);
    #else
    writePartialWord(((BinWord*)m_bytebuf) + wordofst + 1, newval_part2,
                     xceiling(start_bit_in_word, BITS_PER_BYTE));
    #endif
}


void AssembleBinBuf::writeWord(AssembleBinDesc const* desc)
{
    ASSERT0(desc->isWordValid());
    writeWord(desc->bitvalword, desc->bitsize);
}


void AssembleBinBuf::writeBuf(AssembleBinDesc const* desc)
{
    ASSERT0(desc->isBufValid());
    ASSERT0(desc->bitvalbuf);
    writeBuf(desc->bitvalbuf, desc->bitsize);
}


void AssembleBinBuf::writeBuf(BYTE const* bitvalbuf, UINT bitsize)
{
    UINT last_remained = m_cur_bitofst % BIN_WORD_SIZE;
    UINT divisible_word_num = bitsize / BIN_WORD_SIZE;
    if (last_remained != 0) {
        UINT remained_bit_num = bitsize;
        //The number of binword that can be disivible by BIN_WORD_SIZE.
        for (UINT i = 0; i < divisible_word_num; i++) {
            BinWord word = *(((BinWord*)bitvalbuf) + i);
            writeWord(word, BIN_WORD_SIZE);
            remained_bit_num -= BIN_WORD_SIZE;
        }
        if (remained_bit_num != 0) {
            BinWord word = *(((BinWord*)bitvalbuf) + divisible_word_num);
            filterImpurity(word, remained_bit_num);
            writeWord(word, remained_bit_num);
        }
        return;
    }
    UINT byteofst = m_cur_bitofst / BITS_PER_BYTE;
    ::memcpy(m_bytebuf + byteofst, bitvalbuf,
             divisible_word_num * BYTES_PER_WORD);
    m_cur_bitofst += divisible_word_num * BIN_WORD_SIZE;
    UINT remained_bit_num = bitsize - divisible_word_num * BIN_WORD_SIZE;
    if (remained_bit_num != 0) {
        BinWord word = *(((BinWord*)bitvalbuf) + divisible_word_num);
        filterImpurity(word, remained_bit_num);
        writeWord(word, remained_bit_num);
    }
}


void AssembleBinBuf::dump(FILE * h, BYTE const* buf, UINT len)
{
    ::fprintf(h, "\n");
    for (UINT i = 0; i < len; i++) {
        ::fprintf(h, "0x%02x,", buf[i]);
    }
    ::fflush(h);
}


void AssembleBinBuf::dump(FILE * h) const
{
    dump(h, m_bytebuf, m_bytebuflen);
}

} //namespace xcom
