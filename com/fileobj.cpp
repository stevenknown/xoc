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
#include "xcominc.h"

namespace xcom {

//is_del: true to delete the file with same name.
FileObj::FileObj(CHAR const* filename, bool is_del)
{
    ASSERT0(filename);
    if (is_del) { UNLINK(filename); }
    createNew(filename);
    m_file_handler = ::fopen(filename, "rb+");
    ASSERTN(m_file_handler, ("prejudge the file existence"));
    m_is_opened = true;
    m_file_name = filename;
}


void FileObj::createNew(CHAR const* filename)
{
    m_file_handler = ::fopen(filename, "ab+");
    ASSERTN(m_file_handler, ("file IO exception"));
    ::fclose(m_file_handler);
    m_file_handler = nullptr;
}


FileObj::FileObj(FILE * h)
{
    ASSERT0(h);
    m_file_handler = h;
    m_is_opened = false;
    m_file_name = nullptr;
}


FileObj::~FileObj()
{
    if (m_file_handler == nullptr) { return; }
    if (isIOStream()) {
        ::fflush(m_file_handler);
    } else if (m_is_opened) {
        ::fclose(m_file_handler);
    }
    m_file_handler = nullptr;
}


bool FileObj::isFileExist(CHAR const* filename)
{
    ASSERT0(filename);  
    FILE * h = ::fopen(filename, "rb");
    bool exist = false;
    if (h != nullptr) {
        exist = true;
        ::fclose(h);
    }
    return exist;
}


bool FileObj::isIOStream() const
{
    return m_file_handler == stdout;
}


void FileObj::prt(CHAR const* format, ...)
{
    ASSERT0(m_file_handler);
    va_list args;
    va_start(args, format);
    vfprintf(m_file_handler, format, args);
    va_end(args);
    fflush(m_file_handler);
}


size_t FileObj::getFileSize() const
{
    ASSERT0(m_file_handler);
    ::fseek(m_file_handler, 0L, SEEK_END);
    return ::ftell(m_file_handler);
}


FileObj::FO_STATUS FileObj::read(OUT BYTE * buf, size_t offset, size_t size,
                                 OUT size_t * rd)
{
    ASSERT0(buf);
    if (size == 0) { return FO_SUCC; }
    ASSERT0(m_file_handler);
    ::fseek(m_file_handler, (LONG)offset, SEEK_SET);
    //actual_rd is the acutal byte size that read.
    size_t actual_rd = ::fread(buf, 1, size, m_file_handler);
    if (rd != nullptr) { *rd = actual_rd; }
    return FO_SUCC;
}


FileObj::FO_STATUS FileObj::append(BYTE const* buf, size_t size, OUT size_t * wr)
{
    ASSERT0(buf);
    if (size == 0) { return FO_SUCC; }
    ASSERT0(m_file_handler);
    ::fseek(m_file_handler, (LONG)0, SEEK_END);
    //actual_wr is the acutal byte size that wrote.
    size_t actual_wr = ::fwrite(buf, 1, size, m_file_handler);
    if (wr != nullptr) { *wr = actual_wr; }
    return FO_SUCC;
}


FileObj::FO_STATUS FileObj::write(BYTE const* buf, size_t offset, size_t size,
                                  OUT size_t * wr)
{
    ASSERT0(buf);
    if (size == 0) { return FO_SUCC; }
    if (offset > getFileSize()) { return FO_EXCEDE_FILE_END; }
    ASSERT0(m_file_handler);
    ::fseek(m_file_handler, (LONG)offset, SEEK_SET);
    //actual_wr is the acutal byte size that wrote.
    size_t actual_wr = ::fwrite(buf, 1, size, m_file_handler);
    if (wr != nullptr) { *wr = actual_wr; }
    return FO_SUCC;
}

} //namespace xcom
