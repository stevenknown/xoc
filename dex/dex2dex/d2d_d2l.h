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
#ifndef _D2D_D2L_H
#define _D2D_D2L_H

#define  asUInt32(addr) (*((UInt32*)addr))
#define GET_NEWOFFSET(offset)\
    do\
    {\
        offset = getNewOffset(pool, (Int32)(offset));\
        ASSERT0(-1 != (Int32)(offset));\
    }while(0)

#define GET_NEWOFFSET_CHECK(offset)\
    do\
    {\
        Int32 nAddr;\
        nAddr = getNewOffset(pool, (Int32)(offset));\
        ASSERT0(nAddr == (Int32)(offset));\
    }while(0)

#define GET_NEWADDR_CHECK(addr)\
    do\
    {\
        BYTE* nAddr;\
        nAddr = getNewAddr(pool, (BYTE*)(addr));\
        ASSERT0(nAddr == (BYTE*)(addr));\
    }while(0)

#define GET_NEWADDR(addr)\
    do\
    {\
        addr = getNewAddr(pool, (BYTE*)addr);\
    }while(0)

#ifdef __cplusplus
extern "C" {
#endif
    Int32 doDex2Dex(
            DexFile* pDexFile,
            int outFd,
            long* fileLen,
            bool ifOpt,
            char const* dexfilename);
    Int32 doDex2Dex2(
            DexFile* pDexFile,
            unsigned char ** dxbuf,
            unsigned int* dxbuflen,
            unsigned int* cbsHandler,
            char const* dexfilename);
#ifdef __cplusplus
}
#endif


#endif
