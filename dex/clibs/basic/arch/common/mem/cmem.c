/*Copyright 2011 Alibaba Group*/
/*
 *  cmem.c
 *  madagascar
 *
 *  Created by Misa.Z on 2/19/09.
 *  Copyright 2009 Virtualnology. All rights reserved.
 *
 */

#include "mem/cmem.h"
#include "mem/cmemport.h"


static CMemAllocFailProc _allocFailProc = NULL;

void cMemSetAllocFailProc(CMemAllocFailProc proc) {
    _allocFailProc = proc;
}

bool cMemInit() {
    return cMemInitPort();
}

void cMemFinal() {
    cMemFinalPort();
    _allocFailProc = NULL;
}

UInt32 cMemTotalSize(void) {
    return cMemTotalSizePort();
}

UInt32 cMemFreeSize(void) {
    return cMemTotalSize()-cMemUsedSize();
}

UInt32 cMemUsedSize(void) {
    return cMemUsedSizePort();
}

void* cMallocRaw(UInt32 size) {
    void* p = cMallocPort(size);
    if(p == NULL && _allocFailProc) {
        _allocFailProc();
        p = cMallocPort(size);
    }
    return p;
}

void cFree(void* p) {
    cFreePort(p);
}

//** added by raymond

void *cMmap(Int32 fd, Int32 alignment, Int32 size, Int32 mode)
{
    return cMmapPort(fd, alignment, size, mode);
}

void cMunmap(void *address, Int32 length)
{
    return cMunmapPort(address, length);
}
//** end
