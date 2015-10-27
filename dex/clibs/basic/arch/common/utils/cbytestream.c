/*Copyright 2011 Alibaba Group*/
#include "utils/cbytestream.h"
#include "mem/cmem.h"

typedef struct {
    UInt32 capacity;
    UInt32 size;
    UInt32 pos;
    BYTE* data;
}CByteStream;

static const UInt32 cbsDefaultSize = 1024;

CBSHandle cbsInitialize(UInt32 size) {
    CByteStream *bs;

    bs = (CByteStream*)cMalloc(sizeof(CByteStream));
    if(bs == NULL)
        return 0;
    if(size == 0)
        size = cbsDefaultSize;
    bs->data = (BYTE*)cMalloc(size);
    if(bs->data == NULL) {
        cFree(bs);
        return 0;
    }
    cMemset(bs->data, 0, size);
    bs->capacity = size;
    bs->size = 0;
    bs->pos = 0;
    return (CBSHandle)bs;
}

void cbsDestroy(CBSHandle bsh) {
    if(bsh) {
        CByteStream* bs;

        bs = (CByteStream*)bsh;
        if(bs->data)
            cFree(bs->data);
        cFree(bs);
    }
}

static bool expandBuffer(CByteStream* bs, UInt32 needSize) {
    BYTE* buf;
    UInt32 size;
    UInt16 i;

    size = bs->capacity;
    for(i=0; i<31; ++i) {
        size <<= 1;
        if(size >= needSize)
            break;
    }
    if(size < needSize)
        return false;
    buf = (BYTE*)cMalloc(size);
    if(buf == NULL)
        return false;
    cMemset(buf, 0, size);
    cMemcpy(buf, bs->data, bs->size);
    cFree(bs->data);
    bs->data = buf;
    bs->capacity = size;
    return true;
}

Int32 cbsWrite8(CBSHandle bsh, UInt8 d) {
    return cbsWrite(bsh, &d, 1);
}

Int32 cbsWrite16(CBSHandle bsh, UInt16 d) {
    return cbsWrite(bsh, &d, 2);
}

Int32 cbsWrite32(CBSHandle bsh, UInt32 d) {
    return cbsWrite(bsh, &d, 4);
}

Int32 cbsWrite64(CBSHandle bsh, UInt64 d) {
    return cbsWrite(bsh, &d, 8);
}

Int32 cbsWrite(CBSHandle bsh, const void* data, UInt32 size) {
    CByteStream* bs;

    if(bsh == 0)
        return -1;
    bs = (CByteStream*)bsh;
    if(bs->pos + size > bs->capacity) {
        if(!expandBuffer(bs, bs->size + size))
            return -1;
    }
    cMemcpy(bs->data + bs->pos, data, size);
    bs->pos += size;
    if(bs->pos > bs->size)
        bs->size = bs->pos;
    return 0;
}

#define DEFAULT_EXPAND_SIZE 256
Int32 cbsSeek(CBSHandle bsh, UInt32 offset) {
    CByteStream* bs;

    if(bsh == 0)
        return -1;
    bs = (CByteStream*)bsh;

    if(NULL == bs->data)
        return -1;

    if(offset >= bs->capacity) {
        if(!expandBuffer(bs, offset + DEFAULT_EXPAND_SIZE))
            return -1;
    }

    if(offset > bs->size) {
        cMemset(bs->data + bs->size, 0, offset - bs->size);
        bs->size = offset;
    }

    bs->pos = offset;
    return 0;
}

UInt32 cbsGetSize(CBSHandle bsh) {
    CByteStream* bs;

    if(bsh == 0)
        return 0;
    bs = (CByteStream*)bsh;
    return bs->size;
}

UInt32 cbsGetPos(CBSHandle bsh) {
    CByteStream* bs;

    if(bsh == 0)
        return 0;
    bs = (CByteStream*)bsh;
    return bs->pos;
}

BYTE* cbsGetData(CBSHandle bsh) {
    CByteStream* bs;

    if(bsh == 0)
        return NULL;
    bs = (CByteStream*)bsh;
    return bs->data;
}

void cbsCopyData(CBSHandle bsh, void* data, UInt32 bufSize) {
    UInt32 copySize;
    CByteStream* bs;

    if(bsh == 0)
        return;
    bs = (CByteStream*)bsh;
    if(bufSize > bs->size)
        copySize = bs->size;
    else
        copySize = bufSize;
    cMemcpy(data, bs->data, copySize);
}

void cbsReset(CBSHandle bsh) {
    CByteStream* bs;

    if(bsh == 0)
        return;
    bs = (CByteStream*)bsh;
    bs->size = 0;
    bs->pos = 0;
}

bool cbsSetCapacity(CBSHandle bsh, UInt32 size) {
    CByteStream* bs;

    if(bsh == 0)
        return false;
    bs = (CByteStream*)bsh;
    if(bs->capacity >= size)
        return true;
    return expandBuffer(bs, size);
}
