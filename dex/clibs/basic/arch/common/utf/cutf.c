/*Copyright 2011 Alibaba Group*/
/*
 *  cutf.c
 *  madagascar
 *
 *  Created by Misa.Z on 2/28/09.
 *  Copyright 2009 Virtualnology. All rights reserved.
 *
 */

#include "utf/cutf.h"
#include "std/cstd.h"
#include "mem/cmemport.h"

static const BYTE _utf8_bytes_map[] = {
    1,1,1,1,1,1,1,1,
    1,1,1,1,2,2,3,4
};//error encoding char as 1 byte char

#define get_utf8_char_len(c) (_utf8_bytes_map[((UInt8)(c))>>4])


//#ifdef __SYMBIAN32__
//    static const BYTE _specialBytes[] = {0xEF,0xBF,0xBD,0xEF,0xBF,0xBD};
//#endif

UInt32 cUtf8Chars(const char* p, UInt32 size) {
    UInt32 len,count;
    const char* end;
    if(p == NULL) {
        return 0;
    }

    count = 0;
    end = p + size;
    while(p < end) {
        len = get_utf8_char_len(*p);
//TODO: aaron patch this temporary for opera mini plug bug, fix this
//#ifdef __SYMBIAN32__
//        if(*p == 0xEF && p+6 < end) {
//            if(cMemcmp(p, _specialBytes, 6) == 0) {//got /u0000 in unicode for opera mini plugin
//                len = 6;
//            }
//        }
//#endif
        count++;
        p+=len;
    }
    return count;
}

UInt32 cUtf8LengthOfUtf16(const UInt16* p, UInt32 size) {
    const UInt16* end;
    UInt32 len;
    UInt16 c;

    if(p == NULL) {
        return 0;
    }
    end = p + size;
    len = 0;
    while(p < end) {
        c = *p;
        if(c == 0x0) // for 'modified UTF-8', '\0' is 2 bytes. added by liucj
            len += 2;
        else if(c < 0x80) //1byte
            len += 1;
        else if(c < 0x800) //2bytes
            len += 2;
        else //3bytes
            len += 3;
        ++p;
    }
    return len;
}

UInt32 cUtf8ToUtf16(const char* utf8, UInt32 utf8Size, UInt16* utf16, UInt32 utf16Size) {
    UInt32 c,len,pos,idx;

    if(utf8 == NULL || utf16 == NULL)
        return 0;
    len = pos = idx = 0;
    while(idx < utf8Size && pos < utf16Size) {
//TODO: aaron patch this temporary for opera mini plug bug, fix this
//#ifdef __SYMBIAN32__
//        if(*utf8 == 0xEF && idx+6 < utf8Size) {
//            if(cMemcmp(utf8, _specialBytes, 6) == 0) {//got /u0000 in unicode for opera mini plugin
//                idx += 6;
//                utf16[pos] = 0;
//                utf16[pos+1] = 0;
//                pos += 2;
//                continue;
//            }
//        }
//#endif
        len = get_utf8_char_len(*utf8);
        c = (0xFF>>(len)) & (*utf8++);
        idx++;
        len--;
        while(len) {
            c <<= 6;
            c |= (*utf8++)&0x3F;
            ++idx;
            --len;
        }
        utf16[pos] = c & 0xffff;
        ++pos;
    }
    return pos;
}


UInt32 cUtf16ToUtf8(const UInt16* utf16, UInt32 utf16Size, char* utf8, UInt32 utf8Size) {
    UInt32 len,pos,limit,idx,size;
    UInt32 c;

    if(utf16 == NULL || utf8 == NULL)
        return 0;

    len = pos = idx = 0;
    while(idx < utf16Size) {
        c = utf16[idx];
        if(c == 0x0) // for 'modified UTF-8', '\0' is 2 bytes. added by liucj
            len = 2;
        else if(c < 0x80) //1byte
            len = 1;
        else if(c < 0x800) //2bytes
            len = 2;
        else //3bytes
            len = 3;
        if(pos + len > utf8Size)
            break;
        switch(len) {
            case 3: utf8[pos+2] = 0x80 | (c & 0x3f); c = (c >> 6) | 0x800;
            case 2: utf8[pos+1] = 0x80 | (c & 0x3f); c = (c >> 6) | 0xc0;
            case 1: utf8[pos] = (BYTE)c;
        }
        idx++;
        pos += len;
    }
    return pos;
}

UInt16* cUtf16FromUtf8(const char* utf8, UInt32 utf8Size, Int32 *len) {
    UInt16 *ret;
    Int32 sz;

    *len = 0;
    if(utf8 == NULL || utf8Size == 0)
        return NULL;

    sz = cUtf8Chars(utf8, utf8Size);
    if(sz <= 0)
        return NULL;

    ret = (UInt16*)cMallocPort(2*sz);
    if(ret == NULL)
        return NULL;

    sz = cUtf8ToUtf16(utf8, utf8Size, ret, sz*2);
    if(sz <= 0) {
        cFreePort(ret);
        *len = 0;
        return NULL;
    }
    *len = sz/2;
    return ret;
}

// need free with cFree
char* cUtf8FromUtf16(const UInt16* utf16, UInt32 utf16Size, Int32 *len) {
    char* ret;
    Int32 sz;

    *len = 0;
    if(utf16 == NULL || utf16Size == 0)
        return NULL;

    sz = cUtf8LengthOfUtf16(utf16, utf16Size);
    if(sz <= 0)
        return NULL;

    ret = (char*)cMallocPort(sz);
    if(ret == NULL)
        return NULL;

    sz = cUtf16ToUtf8(utf16, utf16Size, ret, sz);
    if(sz <= 0) {
        cFreePort(ret);
        *len = 0;
        return NULL;
    }
    *len = sz;
    return ret;
}

