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
#ifndef _FILEOBJ_H_
#define _FILEOBJ_H_

namespace xcom {

class FileObj {
    COPY_CONSTRUCTOR(FileObj);
public:
    typedef enum {
        FO_SUCC = 0,
        FO_READ_ERROR,
        FO_WRITE_ERROR,
        FO_EXCEDE_FILE_END,
    } FO_STATUS;
    bool m_is_opened;
    FILE * m_file_handler;
    CHAR const* m_file_name;
public:
    //is_del: true to delete the file with same name.
    FileObj(CHAR const* filename, bool is_del = false);
    FileObj(FILE * h);
    ~FileObj();

    //Append binary data to the end of file object.
    //Return the status.
    //buf: write data in buffer to file object.
    //size: the byte size that expect to write.
    //wr: optional, it records the actual byte size that has wrote.
    FO_STATUS append(BYTE const* buf, size_t size, OUT size_t * wr);
    
    FILE * getFileHandler() const { return m_file_handler; }
    size_t getFileSize() const;
    CHAR const* getFileName() const { return m_file_name; }

    //Return true if current file object is IO stream, e.g: stdout.
    bool isIOStream() const;

    //Return true if 'filename' exist.
    static bool isFileExist(CHAR const* filename);

    void prt(CHAR const* format, ...);

    //Read binary data from file object.
    //Return the status.
    //buf: read data and store in the buffer.
    //offset: the byte offset from the begin of file object.
    //size: the byte size that expect to read.
    //rd: optional, it records the actual byte size that has read.
    FO_STATUS read(OUT BYTE * buf, size_t offset, size_t size,
                   OUT size_t * rd = nullptr);

    //Write binary data to file object.
    //Return the status.
    //buf: write data in buffer to file object.
    //offset: the byte offset from the begin of file object.
    //size: the byte size that expect to write.
    //wr: optional, it records the actual byte size that has wrote.
    FO_STATUS write(BYTE const* buf, size_t offset, size_t size,
                    OUT size_t * wr = nullptr);
};

} //namespace xcom
#endif
