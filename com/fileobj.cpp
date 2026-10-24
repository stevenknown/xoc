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
#include "sys/stat.h"

namespace xcom {

CHAR const* g_fo_name[] = {
    "succ",
    "read error",
    "write error",
    "exceed file end",
    "not exist",
    "can not create new file",
};
size_t g_fo_num = sizeof(g_fo_name) / sizeof(g_fo_name[0]);

//
//START FileObj
//
CHAR const* FileObj::getFileStatusName(FO_STATUS st)
{
    ASSERT0(((size_t)st) < g_fo_num);
    return g_fo_name[st];
}


FileObj::FileObj()
{
    m_is_opened = false;
    m_is_readonly = false;
    m_file_name = nullptr;
    m_file_handler = nullptr;
}


void FileObj::init(CHAR const* filename, bool is_del,
                   bool is_readonly, OUT FO_STATUS * st)
{
    ASSERT0(st);
    ASSERT0(filename);
    FO_STATUS tmpst;
    if (st == nullptr) { st = &tmpst; }
    if (is_del) {
        ASSERT0(!is_readonly);
        UNLINK(filename);
    }
    m_is_readonly = is_readonly;
    if (m_is_readonly) {
        *st = openWithReadOnly(filename);
        return;
    }
    *st = openWithWritable(filename);
}


FO_STATUS FileObj::openWithReadOnly(CHAR const* filename)
{
    ASSERT0(m_is_readonly);
    m_file_handler = ::fopen(filename, "rb");
    if (m_file_handler == nullptr) {
        //prejudge the file existence
        return FO_NOT_EXIST;
    }
    m_is_opened = true;
    m_file_name = filename;
    return FO_SUCC;
}


FO_STATUS FileObj::openWithWritable(CHAR const* filename)
{
    ASSERT0(!m_is_readonly);
    FO_STATUS st = createNew(filename);
    if (st != FO_SUCC) { return st; }
    m_file_handler = ::fopen(filename, "rb+");
    ASSERTN(m_file_handler, ("prejudge the file existence"));
    m_is_opened = true;
    m_file_name = filename;
    return FO_SUCC;
}


FO_STATUS FileObj::createNew(CHAR const* filename)
{
    m_file_handler = ::fopen(filename, "ab+");
    if (m_file_handler == nullptr) {
        //file IO exception
        return FO_CAN_NOT_CREATE_NEW_FILE;
    }
    ::fclose(m_file_handler);
    m_file_handler = nullptr;
    return FO_SUCC;
}


FileObj::FileObj(FILE * h)
{
    ASSERT0(h);
    m_file_handler = h;
    m_is_opened = false;
    m_file_name = nullptr;
}


void FileObj::destroy()
{
    if (m_file_handler == nullptr) { return; }
    if (isIOStream()) {
        ::fflush(m_file_handler);
    } else if (m_is_opened) {
        ::fclose(m_file_handler);
    }
    m_is_opened = false;
    m_file_handler = nullptr;
}


bool FileObj::isDirExist(CHAR const* dirname)
{
    struct stat statbuf;
    if (::stat(dirname, &statbuf) != 0) { return false; }
    #ifdef _ON_WINDOWS_
    //#define _S_IFMT   0xF000 // File type mask
    //#define _S_IFDIR  0x4000 // Directory
    //#define _S_IFCHR  0x2000 // Character special
    //#define _S_IFIFO  0x1000 // Pipe
    //#define _S_IFREG  0x8000 // Regular
    //#define _S_IREAD  0x0100 // Read permission, owner
    //#define _S_IWRITE 0x0080 // Write permission, owner
    //#define _S_IEXEC  0x0040 // Execute/search permission, owner
    return HAVE_FLAG(statbuf.st_mode, _S_IFDIR);
    #else
    return S_ISDIR(statbuf.st_mode);
    #endif
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


FO_STATUS FileObj::read(OUT BYTE * buf, size_t offset, size_t size,
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


FO_STATUS FileObj::append(BYTE const* buf, size_t size, OUT size_t * wr)
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


FO_STATUS FileObj::write(BYTE const* buf, size_t offset, size_t size,
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
//END FileObj


//
//START FileMapObj
//
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

FileMapObj::FileMapObj(CHAR const* filename, OUT FO_STATUS * st)
{
    ASSERT0(st);
    m_file_handler = ::fopen(filename, "rb");
    if (m_file_handler == nullptr) {
        //prejudge the file existence
        *st = FO_NOT_EXIST;
        return;
    }
    m_is_opened = true;
    m_file_name = filename;
    #ifdef _ON_WINDOWS_
    *st = initMapByWin();
    #else
    *st = initMapByPosix();
    #endif
    if (*st != FO_SUCC) {
        destroy();
    }
}


FileMapObj::~FileMapObj()
{
    ::munmap(m_map, m_map_size);
    destroy();
}


void FileMapObj::destroy()
{
    if (m_file_handler == nullptr) { return; }
    if (m_is_opened) {
        ::fclose(m_file_handler);
    }
    m_is_opened = false;
    m_file_handler = nullptr;
}


//mmap whole file read-only.
FO_STATUS FileMapObj::initMapByPosix()
{
    ASSERT0(m_is_opened);
    ASSERT0(m_file_handler);
    struct stat sb;
    int fd = ::fileno(m_file_handler);
    if (::fstat(fd, &sb) != 0 || sb.st_size == 0) {
        ::fclose(m_file_handler);
        return FO_MAP_FAILED;
    }
    size_t bytesize = (size_t)sb.st_size;
    m_map = (BYTE*)::mmap(nullptr, bytesize, PROT_READ, MAP_PRIVATE, fd, 0);

    //Even it is safe(mmap keeps reference to the file), we still choose to keep the handler.
    //::fclose(m_file_handler);
    if (m_map == MAP_FAILED) {
        ::fclose(m_file_handler);
        return FO_MAP_FAILED;
    }
    m_map_size = bytesize;

    //HINT:user could hint accesseing memory randomly or sequentially.
    //by MADV_SEQUENTIAL and MADV_RANDOM.
    ::madvise(m_map, bytesize, MADV_RANDOM);
    return FO_SUCC;
}


FO_STATUS FileMapObj::initMapByWin()
{
    //TODO:Use CreateFileMapping + MapViewOfFile to implement mmap.
    //Use UnmapViewOfFile + CloseHandle to implement unmmap.
    //Use OfferVirtualMemory|PrefetchVirtualMemory to implement madvise on
    //Win11.
    //However, there might not be a proper API to implement madvise on Win7.
    ASSERT0(0); //Target Dependent Code.
    return FO_SUCC;
}
//END FileMapObj

} //namespace xcom
