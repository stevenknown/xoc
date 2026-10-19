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

author: Su Zhenyu
@*/
#ifndef _UNICODE_H_
#define _UNICODE_H_

namespace xcom {

#define MAX_UNICODE_CODEPOINT_NUM 0x200
#define INVALID_CODEPOINT 0xFFFFFFFFU
#define CODEPOINT_MAX_BYTESIZE 4
#define INVALID_UTF8 0
#define INVALID_RAWBYTE 0xFF

//Check continuation byte is 0b10xxxxxx.
#define IS_CONT_BYTE(b) (((b) & 0xC0) == 0x80)

//Return the unicode code point.
//The function converts unicode code‑point from valid UTF‑8 byte
//stream (RFC 3629 compliant).
//s:utf-8 encoded string.
//out_len:the byte length of utf-8 string of returned codepoint.
//        *out_len = utf8 char byte length;
//Return INVALID_CODEPOINT if error occurred.
//e.g:given ascii char 'A', its unicode code point is 65, written as U+0041.
//NOTE:the function does NOT handle multi‑code‑point iteration, only
//read ONE codepoint.
UINT32 getCodePointByUTF8(CHAR const* s, UINT * out_len);

//Return the valid UTF‑8 byte sequence by encoding given unicode code‑point
//(RFC 3629 compliant).
//cp:input unicode code‑point. Valid range: 0 ~ 0x10FFFF.
//out_buf:caller‑provided output buffer, MUST have at least 4 bytes available.
//        Writes raw UTF‑8 bytes only, NO null‑terminator appended.
//The function returns the number of UTF‑8 bytes written (1‑4) if
//it is successful. Otherwise return 0.
//NOTE:The function doesn't handle UTF‑16 range (0xD800‑0xDFFF), which
//values > 0x10FFFF, and doesn't write 0, caller shall append null
//terminator for C‑string usage.
UINT getUTF8ByCodePoint(UINT32 cp, OUT BYTE * out_buf, UINT outbuflen);

//Map a raw byte binary from input text to the UTF8 format string.
//e.g:given the Chinese's input raw byte is:0xE4,0xB8,0x96, then
//g_byte_to_unicode_utf8string[0xE4] is string: 0xC3,0xA4,0x0, which is
//the UTF8 format of unicode codepoint U+00E4.
//The function returns the UTF-8 string.
CHAR const* getUTF8ByRawByte(BYTE byte_val);

//Map a raw byte binary from input text to the UTF8 format string that
//indicates the related unicode.
//e.g:given the Chinese's input raw byte is:0xE4,0xB8,0x96, then
//outbuf:record the returned string: 0xC3,0xA4,0, which is
//       the UTF8 format of unicode codepoint U+00E4.
//outbuflen:the byte size of string outbuf.
//codepoint:record the unicode codepoint U+00E4.
//The function returns the number of UTF‑8 bytes written (1‑4) if
//it is successful. Otherwise return 0.
UINT getUTF8AndCodePointByRawByte(
    BYTE byte_val, OUT UINT32 & codepoint, OUT BYTE * outbuf, UINT outbuflen);

//Return the raw byte by given unicode code point.
//The function converts unicode code‑point to a raw byte.
//codepoint: the input codepoint.
//e.g:given its unicode code point is 65, written as U+0041, the function
//return ascii char 'A'.
//NOTE:the function does NOT handle multi‑code‑point iteration, only
//read ONE codepoint.
BYTE getRawBYTEByCodePoint(UINT32 codepoint);

} //namespace xcom

#endif //_UNICODE_H
