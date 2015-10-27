/*Copyright 2011 Alibaba Group*/
/*
 *  cmemport.c
 *  madagascar
 *
 *  Created by Misa.Z on 2/19/09.
 *  Copyright 2009 Virtualnology. All rights reserved.
 *
 */

#include "mem/cmemport.h"
#include "errno/cerrno.h"

#include <string.h>
#include <stdlib.h>

#include <Windows.h>

bool cMemInitPort() {
    return true;
}

void cMemFinalPort() {
}

void* cMallocPort(UInt32 size) {
    void* ret = NULL;
#ifdef WIN32
    printf("");//yangyang added not to crash in windows
#endif
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
    HANDLE mmaping;
    void *result;
    Int32 prot;
    Int32 mapmode;

    switch (mode) {
        case 1:
            prot = PAGE_READONLY;
            mapmode = FILE_MAP_READ;
            break;
        case 2:
            prot = PAGE_READWRITE;
            mapmode = FILE_MAP_WRITE;
            break;
        case 4:
            prot = PAGE_WRITECOPY;
            mapmode = FILE_MAP_COPY;
            break;
        default:
            return ((void *) -1);
    }

    mmaping = CreateFileMapping((HANDLE) fd, NULL, prot, 0, 0, NULL);

    if (mmaping == NULL)
        return ((void *) -1);

    result = MapViewOfFile(mmaping, mapmode, (DWORD) ((alignment >> 0x20) & 0x7fffffff), (DWORD) (alignment & 0xffffffff), (SIZE_T) (size & 0xffffffff));
    CloseHandle(mmaping);

    return result;
}

void cMunmapPort(void *address, Int32 length)
{
    UnmapViewOfFile((HANDLE) address);
}

