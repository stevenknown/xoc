/*@
Copyright (c) 2013-2021, Su Zhenyu steven.known@gmail.com
All rights reserved.

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

THIS SOFTWARE IS PROVIDED "AS IS" AND ANY
EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
@*/
#ifndef _STRBUF_H_
#define _STRBUF_H_

#define MAX_INIT_SIZE 4096

namespace xcom {

class StrBuf {
    COPY_CONSTRUCTOR(StrBuf);
public:
    CHAR * buf; //the buffer that holds the string.

    //The byte length of the buffer. Note the buffer may longer
    //than the string needed.
    UINT buflen;
public:
    //initsize: the byte size that string buffer expected to be.
    //Note the buffer may be longer than a string needed.
    StrBuf(UINT initsize)
    {
        ASSERT0(initsize > 0 && initsize < MAX_INIT_SIZE);
        buflen = initsize;
        buf = (CHAR*)::malloc(initsize);
        buf[0] = 0;
    }
    ~StrBuf() { if (buf != nullptr) { ::free(buf); } }

    void clean()
    {
        ASSERT0(buf);
        buf[0] = 0;
    }
    void copy(StrBuf const& src)
    {
        if (buflen < src.buflen) {
            buflen = src.buflen;
            ASSERT0(buf);
            ::free(buf);
            buf = (CHAR*)::malloc(buflen);
        }
        buf[0] = 0;
        ASSERT0(src.buf);
        ::memcpy(buf, src.buf, buflen);
    }

    //Find sub-string in current string buffer, return the index if
    //sub-string found, otherwise return -1.
    //substring: partial string.
    LONG findSubStr(CHAR const* substring);

    //Return the string buffer byte length.
    UINT getBufLen() const { return buflen; }

    //Return the string buffer.
    CHAR const* getBuf() const { return buf; }

    //The function convert string content into binary.
    //Note the content in given buf must be string format of hex, that is the
    //string can only contain "abcdefABCDEF0123456789".
    //outputbuf: the byte buffer that record the hex number.
    //bufl: byte length of 'outputbuf'.
    void toByteHex(OUT BYTE * outputbuf, UINT bufl);

    //String comparation.
    //Return true if s equal to current string.
    bool is_equal(CHAR const* s) const { return ::strcmp(buf, s) == 0; }

    //Return true if string s is only contain one character t.
    static bool is_equal(CHAR const* s, CHAR t) { return s[0] == t; }

    //Return true if string is empty.
    bool is_empty() const { return buf == nullptr || buf[0] == 0; }

    //Grow buffer to the byte length that no less than 'len'.
    //Note the function will clobber the content in original buffer.
    void growBuf(UINT len)
    {
        if (buflen >= len) { return; }
        buflen = len;
        ASSERT0(buf);
        ::free(buf);
        buf = (CHAR*)::malloc(buflen);
        buf[0] = 0;
    }

    //Composes a string that formed by 'format'.
    void sprint(CHAR const* format, ...);

    //This function print string according to 'format'.
    //args: a list of argument store in stack.
    void vsprint(CHAR const* format, va_list args);

    //Concatenate original string and new strings.
    //Appends a copy of the source string to the current string buffer,
    //the new string is consist of original string and the string formed
    //by 'format'.
    void strcat(CHAR const* format, ...);

    //Concatenate another string to current string.
    void strcat(StrBuf const& another);

    //Concatenate original string and new strings.
    //Appends a copy of the source string to the current string buffer,
    //the new string is consist of original string and the string formed
    //by 'format'.
    //bytesize: the maximum possible byte size of string.
    void strcat(UINT bytesize, CHAR const* format, va_list args);

    //Return byte size of current string.
    //Note the size does NOT include the end-character '\0'.
    size_t strlen() const { return ::strlen(buf); }

    //The functions snprintf() and vsnprintf() do not write more than size
    //bytes (including the terminating null byte ('\0')).
    //bytesize: the maximum possible byte size of string.
    void nstrcat(UINT bytesize, CHAR const* format, ...);

    //Concatenate original string and new strings.
    //Appends a copy of the source string to the current string buffer,
    //the new string is consist of original string and the string formed
    //by 'args'.
    void vstrcat(CHAR const* format, va_list args);
};


//The class represent a vector that record a set of StrBuf.
//Note the class maintains the memory of each StrBuf in the vector, and
//guarantees all allocated memory will be freed during destruction.
class StrBufVec {
    COPY_CONSTRUCTOR(StrBufVec);
protected:
    Vector<StrBuf*> m_strbufvec;
public:
    StrBufVec() {}
    ~StrBufVec()
    {
        for (UINT i = 0; i < m_strbufvec.get_elem_count(); i++) {
            StrBuf * strbuf = m_strbufvec[i];
            if (strbuf != nullptr) { delete strbuf; }
        }
    }

    //Get the No.'n' StrBuf in the vector.
    //Return null if it does not exist.
    StrBuf * getStrBuf(UINT n) const { return m_strbufvec.get(n); }

    //Get the No.'n' StrBuf in the vector. The function will allocate a new
    //StrBuf if it does not exist.
    //bufinitsize: the byte size to initialize new StrBuf.
    StrBuf * genStrBuf(UINT n, UINT bufinitsize = 8)
    {
        StrBuf * strbuf = m_strbufvec.get(n);
        if (strbuf == nullptr) {
            strbuf = new StrBuf(bufinitsize);
            m_strbufvec.set(n, strbuf);
        }
        return strbuf;
    }
    UINT getStrBufNum() const { return m_strbufvec.get_elem_count(); }
};


//
//START FixedStrBuf
//
template <UINT ByteSize>
class FixedStrBuf {
    COPY_CONSTRUCTOR(FixedStrBuf);
protected:
    CHAR m_fixbuf[ByteSize];
    StrBuf * m_strbuf;
protected:
    void allocStrBuf(UINT sz)
    {
        if (m_strbuf != nullptr) { return; }
        m_strbuf = new StrBuf(sz);
    }

    //The function move the string content from fixed-buffer to StrBuf.
    //sz: the byte size that needed to move from fixed-buffer to StrBuf.
    void moveToStrBuf(size_t sz)
    {
        ASSERT0(m_strbuf);
        ASSERT0(m_strbuf->getBufLen() >= ByteSize);
        ASSERT0(sz < ByteSize);
        ::memcpy(m_strbuf->buf, m_fixbuf, sz);
        m_strbuf->buf[sz] = 0;
    }

    UINT strlen() const
    {
        if (m_strbuf == nullptr) { return ::strlen(m_fixbuf); }
        return m_strbuf->strlen();
    }
public:
    FixedStrBuf()
    {
        ASSERT0(ByteSize > 0);
        m_fixbuf[0] = 0;
        m_strbuf = nullptr;
    }
    ~FixedStrBuf() { destroy(); }

    //The function binds a given StrBuf object as the internal StrBuf of
    //current FixedStrBuf. Then all string operations will apply on the given
    //StrBuf object.
    //NOTE: the binded StrBuf object should be unbinded before destroying
    //current FixedStrBuf, otherwise it might be double freed.
    void bind(MOD StrBuf * src);

    void clean()
    {
        if (m_strbuf != nullptr) { m_strbuf->clean(); }
        else { m_fixbuf[0] = 0; }
    }
    void copy(FixedStrBuf const& src);

    void destroy()
    { if (m_strbuf != nullptr) { delete m_strbuf; m_strbuf = nullptr; } }

    //Find sub-string in current string buffer, return the index if
    //sub-string found, otherwise return -1.
    //substring: partial string.
    LONG findSubStr(CHAR const* substring);

    CHAR const* getBuf() const
    { return m_strbuf != nullptr ? m_strbuf->getBuf() : m_fixbuf; }
    CHAR const* getBufLen() const
    { return m_strbuf != nullptr ? m_strbuf->getBufLen() : ByteSize; }

    //Composes a string that formed by 'format'.
    void sprint(CHAR const* format, ...);
    void strcat(UINT bytesize, CHAR const* format, va_list args);
    void strcat(CHAR const* format, ...);

    //The function unbinds StrBuf if exist, and return the unbinded StrBuf
    //object.
    //NOTE: The function regards StrBuf as an external object that input by
    //user.
    StrBuf * unbind();
};


template <UINT ByteSize>
void FixedStrBuf<ByteSize>::bind(MOD StrBuf * src)
{
    ASSERT0(src);
    destroy();
    m_strbuf = src;
}


template <UINT ByteSize>
StrBuf * FixedStrBuf<ByteSize>::unbind()
{
    StrBuf * strbuf = m_strbuf;
    m_strbuf = nullptr;
    return strbuf;
}


template <UINT ByteSize>
void FixedStrBuf<ByteSize>::sprint(CHAR const* format, ...)
{
    clean();
    va_list args;
    va_start(args, format);
    va_list org_args;
    va_copy(org_args, args);
    UINT l = VSNPRINTF(nullptr, 0, format, args);
    strcat(l, format, org_args);
    va_end(args);
    va_end(org_args);
}


template <UINT ByteSize>
void FixedStrBuf<ByteSize>::copy(FixedStrBuf const& src)
{
    if (src.m_strbuf != nullptr) {
        allocStrBuf(src.m_strbuf->getBufLen());
        m_strbuf->copy(*src.m_strbuf);
        return;
    }
    ::memcpy(m_fixbuf, src.m_fixbuf, ByteSize);
}


template <UINT ByteSize>
void FixedStrBuf<ByteSize>::strcat(UINT bytesize, CHAR const* format,
                                   va_list args)
{
    if (m_strbuf != nullptr) {
        return m_strbuf->strcat(bytesize, format, args);
    }
    size_t sl = ::strlen(m_fixbuf);
    if (ByteSize - sl <= bytesize) {
        allocStrBuf(ByteSize + bytesize);
        moveToStrBuf(sl);
        return m_strbuf->strcat(bytesize, format, args);
    }
    UINT k = VSNPRINTF(&m_fixbuf[sl], bytesize + 1, format, args);
    ASSERT0(k == bytesize);
    DUMMYUSE(k);
}


template <UINT ByteSize>
void FixedStrBuf<ByteSize>::strcat(CHAR const* format, ...)
{
    va_list args;
    va_start(args, format);
    va_list org_args;
    va_copy(org_args, args);
    UINT l = VSNPRINTF(nullptr, 0, format, args);
    strcat(l, format, org_args);
    va_end(args);
    va_end(org_args);
}


template <UINT ByteSize>
LONG FixedStrBuf<ByteSize>::findSubStr(CHAR const* substring)
{
    return m_strbuf != nullptr ? xstrstr(m_strbuf->getBuf(), substring) :
                                 xstrstr(m_fixbuf, substring);
}
//END FixedStrBuf

typedef FixedStrBuf<64> DefFixedStrBuf;

} //namespace xcom
#endif
