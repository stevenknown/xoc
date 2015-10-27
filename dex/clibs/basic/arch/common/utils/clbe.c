/*Copyright 2011 Alibaba Group*/
#include "utils/clbe.h"
#include "mem/cmem.h"

UInt64 cReadLE64(BYTE* p) {
    UInt64 ret = 0;

    /**
    ret |= p[4];
    ret |= p[5] << 8;
    ret |= p[6] << 16;
    ret |= p[7] << 24;
    ret <<= 32;
    ret |= p[0];
    ret |= p[1] << 8;
    ret |= p[2] << 16;
    ret |= p[3] << 24;
    */
    cMemcpy(&ret, p, 8);
    return ret;
}

void cWriteLE32(BYTE* dst, UInt32 num) {
    if(dst == NULL)
        return;
    dst[0] = num & 0xff;
    dst[1] = (num >> 8) & 0xff;
    dst[2] = (num >> 16) & 0xff;
    dst[3] = num >> 24;
}

void cWriteLE16(BYTE* dst, UInt16 num) {
    if(dst == NULL)
        return;
    dst[0] = num & 0xff;
    dst[1] = num >> 8;
}

void cWriteLE64(BYTE* dst, UInt64 num) {
    if(dst == NULL)
        return;
    cMemcpy(dst, &num, 8);
}

UInt64 cReadBE64(BYTE* p) {
    UInt64 ret = 0;

    ret |= p[3];
    ret |= p[2] << 8;
    ret |= p[1] << 16;
    ret |= (UInt32)p[0] << 24;
    ret <<= 32;
    ret |= p[7];
    ret |= p[6] << 8;
    ret |= p[5] << 16;
    ret |= (UInt32)(p[4] << 24);
    return ret;
}

void cWriteBE32(BYTE* dst, UInt32 num) {
    if(dst == NULL)
        return;
    dst[0] = num >> 24;
    dst[1] = (num >> 16) & 0xff;
    dst[2] = (num >> 8) & 0xff;
    dst[3] = num & 0xff;
}

void cWriteBE16(BYTE* dst, UInt16 num) {
    if(dst == NULL)
        return;
    dst[0] = num >> 8;
    dst[1] = num & 0xff;
}

void cWriteBE64(BYTE* dst, UInt64 num) {
    BYTE* p;

    if(dst == NULL)
        return;
    p = (BYTE*)&num;
    dst[0] = p[7];
    dst[1] = p[6];
    dst[2] = p[5];
    dst[3] = p[4];
    dst[4] = p[3];
    dst[5] = p[2];
    dst[6] = p[1];
    dst[7] = p[0];
}
