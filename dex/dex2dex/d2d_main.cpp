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
#include <stdio.h>
#include <malloc.h>
#include <errno.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include "trace/ctrace.h"
#include "libdex/DexFile.h"
#include "libdex/DexClass.h"
#include "libdex/CmdUtils.h"
#include "str/cstr.h"
#include "liropcode.h"
#include "d2d_d2l.h"
#include "d2d_main.h"
#include "utils/cbytestream.h"
#include "cominc.h"

static void DumpClassName(DexFile * dexfile)
{
    unsigned int clsN = dexfile->pHeader->classDefsSize;
    FILE * h = fopen("n.tmp","a+");
    for (unsigned int i = 0; i < clsN; i++) {
       DexClassDef const* pDexClassDef = dexGetClassDef(dexfile, i);
       char const* name = dexGetClassDescriptor(dexfile, pDexClassDef);
       fprintf(h, "\n    \"%s\",", name);
    }
    fclose(h);
}

//'dxbuf': Buffer for dex file memory mapped.
bool d2dEntryBuf(
        unsigned char ** dxbuf,
        unsigned int* dxbuflen,
        unsigned int* cbsHandler,
        char const* dexfilename)
{
    bool result = false;
    Int32 error = 0;

    DexFile * dexfile = dexFileParse((const u1*)*dxbuf,
                                      *dxbuflen,
                                      kDexParseDefault);
    if (dexfile == nullptr) {
        LOGE("LEMUR:dexfile is null: DEX parse failed for \n");
        goto END;
    }

    //DumpClassName(dexfile);
    dexfile->pClassLookup = dexCreateClassLookup(dexfile);

    //TODO free buffer
    error = doDex2Dex2(dexfile, dxbuf, dxbuflen, cbsHandler, dexfilename);
    if (error != 0) {
        goto END;
    }
    result = true;

    LOGI("d2dEntry finished.\n");
END:
    if (dexfile != nullptr) {
        dexFileFree(dexfile);
    }
    return result;
}

void d2dEntryBufFree(unsigned int cbsHandler)
{
    if (cbsHandler != 0) {
        cbsDestroy(cbsHandler);
    }
}

//outputfd: file descriptor of output. If it less than 0,
//    output and input dexfile are the same.
int d2dEntry(int dexfd,
             int outputfd,
             long* filelen,
             bool ifopt,
             const char* dexfilename)
{
    ASSERT0(dexfd >= 0);
    Int32 error = 0;
    DexFile* dexfile = nullptr;
    MemMapping inputmap;
    MemMapping outputmap;
    bool inputmapped = false;
    bool outputmapped = false;
    int fd;

    UnzipToFileResult utfr = kUTFRSuccess;
    lseek(dexfd, 0L, SEEK_SET);
    if (outputfd >= 0) {
        lseek(outputfd, 0L, SEEK_SET);
    }

    if (sysMapFileInShmemWritableReadOnly(dexfd, &inputmap) != 0) {
        error = -1;
        LOGE("error: unable to create a map for dexfile\n");
        goto END;
    }
    inputmapped = true;

    if (outputfd >= 0) {
       //if (sysMapFileInShmemWritableReadOnly(outputfd, &outputmap) != 0) {
       //    error = -1;
       //    LOGE("error: unable to create a map for output file\n");
       //    goto END;
       //}
       //outputmapped = true;
    }

    if (ifopt) {
        inputmap.addr = (u1*)(inputmap.addr) + sizeof(DexOptHeader);
        inputmap.length -= sizeof(DexOptHeader);
    }

    dexfile = dexFileParse((const u1*)(inputmap.addr), inputmap.length, kDexParseDefault);
    if (dexfile == nullptr) {
        LOGE("error: parse dex file failed\n");
        error = -1;
        goto END;
    }

    fd = dexfd;
    if (outputfd >= 0) {
        fd = outputfd;
    }

    dexfile->pClassLookup = dexCreateClassLookup(dexfile);
    error = doDex2Dex(dexfile, fd, filelen, ifopt, dexfilename);

    LOGI("d2dEntry finished.\n");
END:
    if (inputmapped) {
        sysReleaseShmem(&inputmap);
    }

    if (outputmapped) {
        sysReleaseShmem(&outputmap);
    }

    if (dexfile != nullptr) {
        dexFileFree(dexfile);
    }

    return error;
}
