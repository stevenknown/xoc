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
/*
 * Copyright (C) 2014 The Alibaba YunOS Project
 */

/*
 * Functions for interpreting LEB128 (little endian base 128) values
 */

#include "d2d_dexlib.h"
#include "std/cstd.h"

#if 0
/*
 * Reads an unsigned LEB128 value, updating the given pointer to point
 * just past the end of the read value. This function tolerates
 * non-zero high-order bits in the fifth encoded byte.
 */
int readUnsignedLeb128(const UInt8** pStream) {
    const UInt8* ptr = *pStream;
    int result = *(ptr++);

    if (result > 0x7f) {
        int cur = *(ptr++);
        result = (result & 0x7f) | ((cur & 0x7f) << 7);
        if (cur > 0x7f) {
            cur = *(ptr++);
            result |= (cur & 0x7f) << 14;
            if (cur > 0x7f) {
                cur = *(ptr++);
                result |= (cur & 0x7f) << 21;
                if (cur > 0x7f) {
                    /*
                     * Note: We don't check to see if cur is out of
                     * range here, meaning we tolerate garbage in the
                     * high four-order bits.
                     */
                    cur = *(ptr++);
                    result |= cur << 28;
                }
            }
        }
    }

    *pStream = ptr;
    return result;
}
#endif
int signedLeb128Size(Int32 value)
{
    int remaining = value >> 7;
    int count = 0;
    bool hasMore = true;
    int end = ((value & (MIN_INT)) == 0) ? 0 : -1;

    while (hasMore) {
        hasMore = (remaining != end)
            || ((remaining & 1) != ((value >> 6) & 1));

        value = remaining;
        remaining >>= 7;
        count++;
    }

    return count;
}

void writeSignedLeb128(Int8* ptr,  Int32 value)
{
    int remaining = value >> 7;
    bool hasMore = true;
    int end = ((value & MIN_INT) == 0) ? 0 : -1;

    while (hasMore) {
        hasMore = (remaining != end)
            || ((remaining & 1) != ((value >> 6) & 1));

        *ptr++ = ((Int8)((value & 0x7f) | (hasMore ? 0x80 : 0)));
        value = remaining;
        remaining >>= 7;
    }

}

/*
 * Returns the number of bytes needed to encode "val" in ULEB128 form.
 */
#if 0
int unsignedLeb128Size(UInt32 data)
{
    int count = 0;

    do {
        data >>= 7;
        count++;
    } while (data != 0);

    return count;
}

/*
 * Writes a 32-bit value in unsigned ULEB128 format.
 *
 * Returns the updated pointer.
 */
UInt8* writeUnsignedLeb128(UInt8* ptr, UInt32 data)
{
    while (true) {
        UInt8 out = data & 0x7f;
        if (out != data) {
            *ptr++ = out | 0x80;
            data >>= 7;
        } else {
            *ptr++ = out;
            break;
        }
    }

    return ptr;
}

#endif
/*
 * Reads a signed LEB128 value, updating the given pointer to point
 * just past the end of the read value. This function tolerates
 * non-zero high-order bits in the fifth encoded byte.
 */
#if 0
int readSignedLeb128(const Int8** pStream) {
    const Int8* ptr = *pStream;
    int result = *(ptr++);

    if (result <= 0x7f) {
        result = (result << 25) >> 25;
    } else {
        int cur = *(ptr++);
        result = (result & 0x7f) | ((cur & 0x7f) << 7);
        if (cur <= 0x7f) {
            result = (result << 18) >> 18;
        } else {
            cur = *(ptr++);
            result |= (cur & 0x7f) << 14;
            if (cur <= 0x7f) {
                result = (result << 11) >> 11;
            } else {
                cur = *(ptr++);
                result |= (cur & 0x7f) << 21;
                if (cur <= 0x7f) {
                    result = (result << 4) >> 4;
                } else {
                    /*
                     * Note: We don't check to see if cur is out of
                     * range here, meaning we tolerate garbage in the
                     * high four-order bits.
                     */
                    cur = *(ptr++);
                    result |= cur << 28;
                }
            }
        }
    }

    *pStream = ptr;
    return result;
}
#endif

#if 0
/*
 * Reads an unsigned LEB128 value, updating the given pointer to point
 * just past the end of the read value and also indicating whether the
 * value was syntactically valid. The only syntactically *invalid*
 * values are ones that are five bytes long where the final byte has
 * any but the low-order four bits set. Additionally, if the limit is
 * passed as non-NULL and bytes would need to be read past the limit,
 * then the read is considered invalid.
 */
int readAndVerifyUnsignedLeb128(const UInt8** pStream, const UInt8* limit,
        bool* okay) {
    const UInt8* ptr = *pStream;
    int result = readUnsignedLeb128(pStream);

    if (((limit != NULL) && (*pStream > limit))
            || (((*pStream - ptr) == 5) && (ptr[4] > 0x0f))) {
        *okay = false;
    }

    return result;
}

/*
 * Reads a signed LEB128 value, updating the given pointer to point
 * just past the end of the read value and also indicating whether the
 * value was syntactically valid. The only syntactically *invalid*
 * values are ones that are five bytes long where the final byte has
 * any but the low-order four bits set. Additionally, if the limit is
 * passed as non-NULL and bytes would need to be read past the limit,
 * then the read is considered invalid.
 */
int readAndVerifySignedLeb128(const UInt8** pStream, const UInt8* limit,
        bool* okay) {
    const UInt8* ptr = *pStream;
    int result = readSignedLeb128(pStream);

    if (((limit != NULL) && (*pStream > limit))
            || (((*pStream - ptr) == 5) && (ptr[4] > 0x0f))) {
        *okay = false;
    }

    return result;
}
#endif
