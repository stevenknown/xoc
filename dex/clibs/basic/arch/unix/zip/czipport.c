/*Copyright 2011 Alibaba Group*/
#include "zip/czipport.h"
#include <zlib.h>
#include "mem/cmem.h"

static z_stream zstrm;

UInt32 cZipCompFlateInit() {
    cMemset(&zstrm, 0, sizeof(zstrm));
    if(deflateInit2(&zstrm, Z_DEFAULT_COMPRESSION, Z_DEFLATED, -MAX_WBITS, 8, Z_DEFAULT_STRATEGY) != Z_OK)
        return 0;
    return 1;
}

void cZipCompFlateEnd(UInt32 id) {
    deflate(&zstrm, Z_FINISH);
    deflateEnd(&zstrm);
}

Int32 cZipDeflate(UInt32 compID, BYTE* in, UInt32 inSize, BYTE* out, UInt32 bufSize, UInt32* outSize) {
    zstrm.next_in = in;
    zstrm.avail_in = inSize;
    zstrm.next_out = out;
    zstrm.avail_out = bufSize;
    if(deflate(&zstrm, Z_NO_FLUSH) != Z_OK)
        return 0;
    *outSize = zstrm.next_out - out;
    return inSize - zstrm.avail_in;
}

Int32 cZipDeflateFinish(UInt32 compID, BYTE* out, UInt32 bufSize, UInt32* outSize) {
    Int32 ret;
    zstrm.next_out = out;
    zstrm.avail_out = bufSize;
    ret = deflate(&zstrm, Z_FINISH);
    *outSize = zstrm.next_out - out;
    if(ret == Z_STREAM_END)
        return 0;
    if(ret == Z_OK)
        return 1;
    return -1;
}

Int32 cZipInflate(UInt32 compID, const BYTE* in, UInt32 inSize, BYTE* out, UInt32 bufSize, UInt32* outSize) {
    return 0;
}
