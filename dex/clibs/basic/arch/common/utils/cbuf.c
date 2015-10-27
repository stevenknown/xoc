/*Copyright 2011 Alibaba Group*/
#include <stdarg.h>
#include "std/cstd.h"
#include "str/cstrport.h"
#include "utils/cbuf.h"
#include "mem/cmem.h"

#define EXPAND_MEM_BLOCK_SIZE 128

int cBufFormatString(CBufferT *mem, const char* format, ...) {
    va_list arg;
    Int32 count;

    if(!mem || !format)
        return -1;
    va_start(arg, format);
    if(mem->size == 0)
        if(!cBufExpand(mem, EXPAND_MEM_BLOCK_SIZE)) {
            va_end(arg);
            return -1;
        }
    count = cVsnprintf(mem->buffer, mem->size, format, arg);
    if((int)mem->size > count) {
        mem->usedLen = count;
        va_end(arg);
        return count;
    }
    if(!cBufExpand(mem, count+1)) {
        va_end(arg);
        return -1;
    }
#ifdef __MTK_TARGET__//for mtk platform only
    va_end(arg);
    va_start(arg, format);
#endif
    count = cVsnprintf(mem->buffer, mem->size, format, arg);
    va_end(arg);
    mem->usedLen = count;
    return count;
}

bool cBufAppend(CBufferT *mem, const void *data, UInt32 len) {
    if(!mem || !data)
        return false;
    if(len == 0)
        return true;
    if(mem->usedLen + len > mem->size)
        if(!cBufExpand(mem, mem->usedLen + len + EXPAND_MEM_BLOCK_SIZE))
            return false;
    cMemcpy(mem->buffer + mem->usedLen, data, len);
    mem->usedLen += len;
    return true;
}

bool cBufAppendString(CBufferT *mem, const char* str) {
    UInt32 len;
    bool ret;

    if(!mem || !str)
        return false;
    len = cStrlen(str);
    ret = cBufAppend(mem, str, len + 1);

    //ignore string null terminate for next append call
    if(ret)
        --mem->usedLen;
    return ret;
}

bool cBufSetValue(CBufferT *mem, const void *data, UInt32 size) {
    if(!mem || !data)
        return false;
    if(mem->size < size)
        if(!cBufExpand(mem, size + EXPAND_MEM_BLOCK_SIZE))
            return false;
    cMemcpy(mem->buffer, data, size);
    mem->usedLen = size;
    return true;
}

bool cBufExpand(CBufferT *mem, int size) {
    char* tmp;

    if(!mem)
        return false;
    if((int)mem->size >= size)
        return true;
    tmp = mem->buffer;
    mem->buffer = (char*)cMalloc(size);
    if(!mem->buffer) {
        mem->buffer = tmp;
        return false;
    }
    cMemset(mem->buffer, 0, size);
    if(tmp != NULL) {
        cMemcpy(mem->buffer, tmp, mem->size);
        cFree(tmp);
    }
    mem->size = size;
    return true;
}

void cBufClear(CBufferT *mem) {
    if(mem == NULL)
        return;
    mem->usedLen = 0;
}

void cBufFree(CBufferT *mem) {
    if(!mem)
        return;
    if(mem->buffer) {
        cFree(mem->buffer);
        mem->buffer = NULL;
    }
    mem->size = 0;
    mem->usedLen = 0;
}
