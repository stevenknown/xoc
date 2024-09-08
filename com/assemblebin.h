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
#ifndef __ASSEMBLE_BIN_H__
#define __ASSEMBLE_BIN_H__

namespace xcom {

#define BIN_WORD_SIZE 64
#define BYTES_PER_WORD (BIN_WORD_SIZE / BITS_PER_BYTE)

typedef UINT64 BinWord;

//The class describes binary value in Word Mode or Buffer Mode.
//If given binary bit size is less than BIN_WORD_SIZE, the binary will be in
//Word Mode, otherwise in Buffer Mode.
class AssembleBinDesc {
public:
    UINT bitsize;
    union {
        BinWord bitvalword;
        BYTE const* bitvalbuf;
    };
    bool isWordValid() const { return bitsize <= BIN_WORD_SIZE; }
    bool isBufValid() const { return !isWordValid(); }
public:
    //bs: bit size of given binary value.
    //w: store the binary value.
    //e.g:if we describe binary value 0x3, then bs is 2, w is 0x3.
    AssembleBinDesc(UINT bs, BinWord w) : bitsize(bs), bitvalword(w) {}

    //bs: bit size of given binary value.
    //buf: a buffer that hold the binary value. Note user should have to
    //     guarantee that 'bs' and 'buf' match each other.
    //e.g:if we describe binary value 0xAAAABBBBCCCCDDDDEE in 'buf',
    //then bs should be 72.
    //Note if the binary bit size is less than BIN_WORD_SIZE, the constructor
    //will describe binary in Word Mode.
    AssembleBinDesc(UINT bs, BYTE const* buf) : bitsize(bs)
    {
        if (isWordValid()) {
            ASSERTN(bs % BITS_PER_BYTE == 0,
                    ("bitsize must be interal multiple of BYTE"));
            bitvalword = 0;
            ::memcpy(&bitvalword, buf, bs / BITS_PER_BYTE);
            return;
        }
        bitvalbuf = buf;
    }
    UINT getBitSize() const { return bitsize; }
    UINT getByteSize() const { return xceiling((INT)bitsize, BITS_PER_BYTE); }
};


//The class is used to record a vector of sorted Binary Descriptors of
//assembled binary. Note the bit-value recorded by each BinDesc will be
//written in Buffer in order.
class AssembleBinDescVec : public Vector<AssembleBinDesc> {
public:
    UINT getTotalBitSize() const
    {
        UINT totalbitsize = 0;
        for (UINT i = 0; i < get_elem_count(); i++) {
            totalbitsize += (*this)[i].getBitSize();
        }
        return totalbitsize;
    }
    UINT getTotalByteSize() const
    {
        UINT bs = getTotalBitSize();
        return xcom::xceiling((INT)bs, BITS_PER_BYTE);
    }
};


//The class converts string style hex number into binary style hex.
//e.g: string style hex numbers is "A0B1", the class will output hex number
//0xA0 and 0xB1 that stored continuously in output buffer.
//Note there should NOT have blank bewteen string style hex, such as "A0 B1".
//USAGE:
//  AssembleBinDescVec vec;
//
//  #Add a field that consist of 3 bits, and the value is 0x7.
//  vec.append(AssembleBinDesc(3, 0x7));
//
//  #Add a field that consist of 72 bits, and the bits value stored in an array.
//  BYTE arr[] = {1, 2, 3, 4, 5, 6, 7, 8, 9};
//  vec.append(AssembleBinDesc(72, arr));
//
//  #Assemble binary and store the result in 'outbuf'.
//  BYTE outbuf[100];
//  AssembleBinBuf bufbin(outbuf, 100, vec);
class AssembleBinBuf {
    UINT m_cur_bitofst;
    UINT m_bytebuflen;
    BYTE * m_bytebuf;
public:
    //buf: the output buffer that hold the binary.
    //buflen: the byte length of 'buf'.
    //descvec: a vector that describe each bin-field.
    AssembleBinBuf(OUT BYTE * buf, UINT buflen,
                   AssembleBinDescVec const& descvec);

    static void dump(FileObj & fo, BYTE const* buf, UINT len);
    void dump(FileObj & fo) const;

    //Return true if given buffer size is big enough to hold all bits
    //in descvec.
    //expectbuflen: record the expected buffer length at least in usage.
    static bool isBufBigEnough(UINT bytebuflen,
                               AssembleBinDescVec const& descvec,
                               OUT UINT * expectbuflen = nullptr);

    //Return true if 'word' contains non-zero value beyond 'bitsize' number
    //of bit, namely impurity.
    //e.g:If given bitsize is 2, but the value in 'word' is 0x7, the function
    //will return true.
    static bool isImpure(BinWord word, UINT bitsize);

    //Remove the impurity.
    static void filterImpurity(BinWord & word, UINT bitsize);

    //Write a set of bit into binary buffer.
    //word: hold the bit value.
    //bitsize: the number of bit to write.
    void writeWord(BinWord word, UINT bitsize);
    void writeWord(AssembleBinDesc const* desc);

    //Write a set of bit into binary buffer.
    //bitvalbuf: hold the bit value.
    //bitsize: the number of bit to write.
    void writeBuf(BYTE const* bitvalbuf, UINT bitsize);
    void writeBuf(AssembleBinDesc const* desc);
};

} //namespace xcom

#endif //END __ASSEMBLE_BIN_H__
