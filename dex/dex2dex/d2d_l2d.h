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

author: GongKai, JinYue
@*/
#ifndef _D2D_L2D_H
#define _D2D_L2D_H

#include "utils/cbytestream.h"

typedef struct D2Dpool D2Dpool;
typedef struct D2DfixAddr D2DfixAddr;
typedef struct D2DdexInstr D2DdexInstr;
typedef struct D2DdexInstrList D2DdexInstrList;
typedef struct TargetInsnStruct TargetInsn;

struct D2Dpool
{
    CBSHandle lbs;
    CBSHandle otherLbs;
    CBSHandle classDataCbs;
    CBSHandle mapLbs;

    UInt32 currentSize;
    UInt32 codeOff;
    UInt32 codeItemOff;
    UInt32 codeItemSize;
    UInt32 classDataOffset;
    UInt32 constSize;

    DexHeader* header;

    UInt32 stringIdsOff;
    UInt32 typeIdsOff;
    UInt32 protoIdOff;
    UInt32 methodIdOff;
    UInt32 fieldIdOff;
    UInt32 classEntryOff;
    UInt32 mapOff;
    UInt32 linkOff;
    UInt32  dataOff;
    UInt32 dataSize;

    UInt32 dexDebugItemOff;
    UInt32 debugItemOff;

    UInt32 dexTypeListOff;
    UInt32 typeListOff;

    UInt32 dexAnnotationSetRefListOff;
    UInt32 annotationSetRefListSize;
    UInt32 annotationSetRefListOff;

    UInt32 dexAnnotationSetOff;
    UInt32 annotationSetSize;
    UInt32 annotationSetOff;

    UInt32 dexStringDataOff;
    UInt32 stringDataSize;
    UInt32 stringDataOff;

    UInt32 dexAnnotationItemOff;
    UInt32 annotationItemOff;

    UInt32 dexEncodedArrayOff;
    UInt32 encodedArrayOff;

    UInt32 dexAnnotationsDirectoryOff;
    UInt32 annotationsDirectorySize;
    UInt32 annotationsDirectoryOff;
    UInt32 updateClassDataSize;

};

struct D2DfixAddr
{
    UInt32 baseSaddr; /*the code item's start addr offset from file start*/
    UInt32 baseEaddr; /*the code item's end addr offset from file start*/

    Int32 fixLen;

    D2DfixAddr* next;
};

struct D2DdexInstr{
    BYTE*  instrData;
    UInt32 instrSize;
    UInt32 instrOffset;
};

struct D2DdexInstrList
{
    UInt32 instrCount;
    D2DdexInstr* instr;
};

struct TargetInsnStruct{
    UInt32 num;
    UInt32 offset;
    UInt32 targetNum;
    UInt32 targetOffset;
    UInt32 size;
    UInt32 lastfixSize;
};

typedef struct D2Dleb128{
    UInt32 size;
    void* datas;
}D2Dleb128;

#ifdef __cplusplus
extern "C" {
#endif
   // Int32 l2dWithAot(D2Dpool* pool, const DexCode* pCode, LIRCode* code);
   void writeSignedLeb128ToCbs(CBSHandle handle, Int32 data);
   Int32 writeUnSignedLeb128ToCbs(CBSHandle handle, UInt32 data);
   void lir2dexCode(D2Dpool* pool, const DexCode* dexCode, LIRCode* lircode);
   void lir2dexCode_orig(D2Dpool* pool, const DexCode* pCode, LIRCode* code);
   UInt32 gdb_compute_dataSize(D2Dpool* pool);
   void alignLbs(CBSHandle lbs);
   void aligmentBy4Bytes(D2Dpool* pool);
   Int32 transformCode(LIRCode const* code, DexCode* nCode);
   DexCode * writeCodeItem(D2Dpool* pool, CBSHandle cbsCode,
                           UInt16 registersSize,
                           UInt16 insSize,
                           UInt16 outsSize,
                           UInt16 triesSize,
                           UInt32 debugInfoOff,
                           UInt32 insnsSize);
#ifdef __cplusplus
}
#endif

#endif
