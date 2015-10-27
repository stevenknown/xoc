/*Copyright 2011 Alibaba Group*/
/*
 *  cmemport.c
 *  madagascar
 *
 *  Created by Misa.Z on 2/19/09.
 *  Copyright 2009 Virtualnology. All rights reserved.
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>

#include "mem/cmemport.h"
#include "errno/cerrno.h"

bool cMemInitPort() {
    return true;
}

void cMemFinalPort() {
}

void* cMallocPort(UInt32 size) {
    void* ret = NULL;

    ret = malloc(size);
    if(ret == NULL) {
        _cerrno = ERROR_MEM_ALLOC;
        return NULL;
    }

    return ret;
}

void cFreePort(void* p)  {
    if(p == NULL)
        return;

    free(p);
}

UInt32     cMemTotalSizePort(void) {
    return 64*1024*1024;
}

UInt32     cMemUsedSizePort(void) {
    return 1*1024*1024;
}




void *cMmapPort(Int32 fd, Int32 alignment, Int32 size, Int32 mode)
{
    void *result;
    Int32 prot;
    Int32 flags;

    switch (mode) {
        case 1:
            prot = PROT_READ;
            flags = MAP_SHARED;
            break;
        case 2:
            prot = PROT_READ | PROT_WRITE;
            flags = MAP_SHARED;
            break;
        case 4:
            prot = PROT_READ;
            flags = MAP_PRIVATE;
            break;
        default:
            return NULL;
    }
    result = mmap(NULL, (size_t) (size & 0x7fffffff), prot, flags, fd, (off_t) (alignment & 0x7fffffff));
    if (result == MAP_FAILED)
        return NULL;

    return result;
}

void cMunmapPort(void *address, Int32 length)
{
    munmap(address, length);
}




