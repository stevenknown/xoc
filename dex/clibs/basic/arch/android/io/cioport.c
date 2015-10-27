/*Copyright 2011 Alibaba Group*/
/*
 *  ioport.c
 *  madagascar
 *
 *  Created by Misa.Z on 2/22/09.
 *  Copyright 2009 Virtualnology. All rights reserved.
 *
 */

#include "io/cioport.h"
#include "mem/cmem.h"
#include "errno/cerrno.h"
#include "utils/cbuf.h"
#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>
#include <string.h>
#include <dirent.h>
#include <stdio.h>

#include <errno.h>
//LEMUR_PROJECT    start
/*<2010.01.21 add by jonezheng for compile windows lexgen.exe.*/
#ifdef HAVE_MS_C_RUNTIME
//stat.h
#define S_IFMT 00170000
#define S_IFREG 0100000
#define S_IFDIR 0040000

#define S_ISREG(m) (((m) & S_IFMT) == S_IFREG)
#define S_ISDIR(m) (((m) & S_IFMT) == S_IFDIR)

#define S_IRUSR 00400
#define S_IWUSR 00200

#define S_IRGRP 00040
#define S_IROTH 00004

#endif//HAVE_MS_C_RUNTIME
/*>2010.01.21 add by jonezheng for compile windows lexgen.exe.*/
//LEMUR_PROJECT    end

Int32 cIOOpenPort(const char* fname, UInt32 of) {
    Int32 flags = 0;

    if(fname == NULL || fname[0] == 0)
        return -1;

    if(of & MADA_OF_READ) {
        if(of & MADA_OF_WRITE)
            flags |= O_RDWR;
        else
            flags |= O_RDONLY;
    }
    else if(of & MADA_OF_WRITE)
        flags |= O_WRONLY;

    if(of & MADA_OF_APPEND)
        flags |= O_APPEND;

    if(of & MADA_OF_TRUNC) {
        flags |= O_TRUNC;
    }

    if(of & MADA_OF_CREATE) {
        flags |= O_CREAT;
    }
//LEMUR_PROJECT    start
/*<2010.01.21 add by jonezheng for compile windows lexgen.exe, open file OD OA error.*/
#ifdef HAVE_MS_C_RUNTIME
        flags |= O_BINARY;
#endif//HAVE_MS_C_RUNTIME
/*>2010.01.21 add by jonezheng for compile windows  lexgen.exe,open file OD OA error.*/
//LEMUR_PROJECT    end
    //int fd = 0;
    //fd =  open(fname, flags, S_IWUSR|S_IRUSR|S_IRGRP|S_IROTH);
    //printf("fd=%d,errno=%d\n",fd,errno);
    //return fd;
    return open(fname, flags, S_IWUSR|S_IRUSR|S_IRGRP|S_IROTH);
}

void cIOClosePort(Int32 fd) {
    if(fd < 0)
        return;

    close(fd);
}

Int32 cIOReadPort(Int32 fd, void* buf, UInt32 bufSize) {
    if(fd < 0) {
        _cerrno = ERROR_IO_ID;
        return -1;
    }
    if(buf == NULL) {
        _cerrno = ERROR_MEM_NULL_OBJ;
        return -1;
    }
    return read(fd, buf, bufSize);
}

Int32 cIOWritePort(Int32 fd, const void* buf, UInt32 bufSize) {
    if(fd < 0) {
        _cerrno = ERROR_IO_ID;
        return -1;
    }
    if(buf == NULL) {
        _cerrno = ERROR_MEM_NULL_OBJ;
        return -1;
    }
    return write(fd, buf, bufSize);
}

Int32 cIOSeekPort(Int32 fd, Int32 offset, UInt8 sf) {
    Int32 flags = SEEK_SET;

    if(fd < 0) {
        _cerrno = ERROR_IO_ID;
        return -1;
    }

    if(sf == SK_SET)
        flags = SEEK_SET;
    else if(sf == SK_CUR)
        flags = SEEK_CUR;
    else if(sf == SK_END)
        flags = SEEK_END;


    return lseek(fd, offset, flags);
}

static UInt32 mapAttributes(UInt16 mode) {
    UInt32 attrs = 0;

    if (S_ISDIR(mode)) {
        attrs |= MDFS_ATTR_DIR;
    }

    if (S_ISREG(mode)) {
        attrs |= MDFS_ATTR_ARCHIVE;
    }

    if ((mode & S_IRUSR)) {
        attrs |= MDFS_ATTR_READ;
    }

    if ((mode & S_IWUSR)) {
        attrs |= MDFS_ATTR_WRITE;
    }

    return attrs;
}

bool cIOGetFileInfoPort(const char* fname, FileInfoT* info) {
    struct stat st;
    if(fname == NULL || info == NULL) {
        _cerrno = ERROR_MEM_NULL_OBJ;
        return false;
    }
    if(stat(fname, &st) != 0)
        return false;
    info->size = st.st_size;
    info->lastModified = st.st_mtime;
    info->fileAttributes = mapAttributes(st.st_mode);
    return true;
}

bool cIOGetFDInfoPort(Int32 fd, FileInfoT* info) {
    struct stat st;

    if(info == NULL) {
        _cerrno = ERROR_MEM_NULL_OBJ;
        return false;
    }
    if(fd < 0) {
        _cerrno = ERROR_IO_ID;
        return false;
    }

    if(fstat(fd, &st) != 0)
        return false;

    info->size = st.st_size;
    info->lastModified = st.st_mtime;
    info->fileAttributes = mapAttributes(st.st_mode);
    return true;
}

bool cIOSetFileAttributesPort(const char* fname, UInt32 attrs) {
    return false;
}

bool cIOExistPort(const char* fname) {
    if(fname == NULL || fname[0] == 0)
        return false;
    if(access(fname,R_OK) == 0)
        return true;
    return false;
}

bool cIOMakeDirPort(const char* path) {
    char *tmp;
    int sz, i = 0;

    if(path == NULL || path[0] == 0)
        return false;
    sz = strlen(path)+1;
    tmp = (char*)cMalloc(sz);
    if(!tmp)
        return false;
    cMemset(tmp, 0, sz);
    while(path[i] != 0) {
        if(path[i] == '/') {
            cMemcpy(tmp, path, i+1);
            if(!cIOExistPort(tmp)) {
//LEMUR_PROJECT    start
/*<2010.01.21 add by jonezheng for compile windows lexgen.exe.*/
#ifdef HAVE_MS_C_RUNTIME
                if(_mkdir(tmp) != 0)
                    goto failed;
#else//HAVE_MS_C_RUNTIME
                if(mkdir(tmp, S_IRWXU | S_IRGRP | S_IXGRP | S_IROTH | S_IXOTH) != 0)
                    goto failed;
#endif//    HAVE_MS_C_RUNTIME
/*>2010.01.21 add by jonezheng for compile windows lexgen.exe.*/
//LEMUR_PROJECT    end
            }
        }
        ++i;
    }
    if(!cIOExistPort(path))
//LEMUR_PROJECT    start
/*<2010.01.21 add by jonezheng for compile windows lexgen.exe.*/
#ifdef HAVE_MS_C_RUNTIME
        if(_mkdir(path) != 0)
            goto failed;
#else//HAVE_MS_C_RUNTIME
        if(mkdir(path, S_IRWXU | S_IRGRP | S_IXGRP | S_IROTH | S_IXOTH) != 0)
            goto failed;
#endif//    HAVE_MS_C_RUNTIME
/*>2010.01.21 add by jonezheng for compile windows lexgen.exe.*/
//LEMUR_PROJECT    end
    cFree(tmp);
    return true;

failed:
    cFree(tmp);
    return false;
}

//LEMUR_PROJECT    start
/*<2010.01.21 add by jonezheng for complile windows lexgen.exe.*/
#ifdef HAVE_MS_C_RUNTIME
typedef struct STDIR
{
    DIR *dir;
    char rootPath[1024];
}DIR_T;
#endif//HAVE_MS_C_RUNTIME
/*>2010.01.21 add by jonezheng for compile windows lexgen.exe.*/
//LEMUR_PROJECT    end


DirHandleT cIOOpenDirPort(const char* path, const char* matchPattern) {
//LEMUR_PROJECT    start
/*<2010.01.21 add by jonezheng for complile windows lexgen.exe.*/
#ifdef HAVE_MS_C_RUNTIME
        DIR_T *pDir = (DIR_T *)malloc(sizeof(DIR_T));
        if(!pDir)
            return NULL;

    if(!path)
    {
        free((void*)pDir);
        return 0;
    }
    pDir->dir = opendir(path);
        strcpy(pDir->rootPath, path);
    return (DirHandleT)pDir;
#else //HAVE_MS_C_RUNTIME
    DIR *dir;

    if(!path)
        return 0;
    dir = opendir(path);
    return (ULong)dir;
#endif//HAVE_MS_C_RUNTIME
/*>2010.01.21 add by jonezheng for compile windows lexgen.exe.*/
//LEMUR_PROJECT    end
}


bool cIOExistsPort(const char* fname) {
    if(access(fname, R_OK) == 0)
        return true;
    return false;
}

bool cIOReadDirPort(DirHandleT dir, DirentT *pDirentInfo) {
    if(dir == 0 || pDirentInfo == NULL)
        return false;

//LEMUR_PROJECT    start
/*<2010.01.21 add by jonezheng for compile windows lexgen.exe.*/
#ifdef HAVE_MS_C_RUNTIME
    struct stat st;
    char fullPath[1024] = "";
    DIR_T *pDir = (DIR_T *)dir;
    struct dirent *de = readdir((DIR*)pDir->dir);
    int rootpathLen = strlen(pDir->rootPath);

    if (NULL == de) {
        return false;
    }
    memset(pDirentInfo, 0, sizeof(DirentT));

    if( (rootpathLen > 0) && ( '/' != pDir->rootPath[rootpathLen -1]) )
    {
        snprintf(fullPath,1024,"%s/%s",pDir->rootPath,de->d_name);
    }
    else
    {
        snprintf(fullPath,1024,"%s%s",pDir->rootPath,de->d_name);
    }

    if(0 != stat(fullPath, &st))
    {
        return false;
    }

    pDirentInfo->fileAttributes = mapAttributes(st.st_mode);
#else //HAVE_MS_C_RUNTIME
    struct dirent *de = readdir((DIR*)dir);
    if (de == NULL) {
        return false;
    }

    memset(pDirentInfo, 0, sizeof(DirentT));

    //pDirentInfo->fileAttributes = getFileAttributes(dir, de);

    if((de->d_type & DT_DIR) > 0)
        pDirentInfo->fileAttributes |= MDFS_ATTR_DIR;
    else if((de->d_type & DT_REG) > 0)
        pDirentInfo->fileAttributes |= MDFS_ATTR_ARCHIVE;
#endif//    HAVE_MS_C_RUNTIME
/*>2010.01.21 add by jonezheng for compile windows lexgen.exe.*/
//LEMUR_PROJECT    end
    strcpy(pDirentInfo->fileName, de->d_name);
    return true;
}

void cIORewindDirPort(DirHandleT dir) {
    if(dir)
    {
//LEMUR_PROJECT    start
/*<2010.01.21 add by jonezheng for complile windows lexgen.exe.*/
#ifdef HAVE_MS_C_RUNTIME
        DIR_T *pDir = ((DIR_T*)dir);

        rewinddir(pDir->dir);
#else //HAVE_MS_C_RUNTIME
        rewinddir((DIR*)dir);
#endif//HAVE_MS_C_RUNTIME
/*>2010.01.21 add by jonezheng for complile windows lexgen.exe.*/
//LEMUR_PROJECT    end
    }
}

void cIOCloseDirPort(DirHandleT dir) {
    if(dir)
    {
//LEMUR_PROJECT    start
/*<2010.01.21 add by jonezheng for complile windows lexgen.exe.*/
#ifdef HAVE_MS_C_RUNTIME
        DIR_T *pDir = ((DIR_T*)dir);

        closedir(pDir->dir);
        free(pDir);
#else //HAVE_MS_C_RUNTIME
        closedir((DIR*)dir);
#endif //HAVE_MS_C_RUNTIME
/*>2010.01.21 add by jonezheng for complile windows lexgen.exe.*/
//LEMUR_PROJECT    end
    }
}

Int32 cIOSizePort(int fd) {
    struct stat st;

    if (fstat(fd, &st) != 0) {
        return -1;
    }

    return st.st_size;
}

bool cIORemovePort(const char* fname) {
    if(fname == NULL || fname[0] == 0)
        return false;
    return (remove(fname) == 0);
}

bool cIORenamePort(const char*oldName,const char* newName) {
    if(oldName == NULL || newName == NULL)
        return false;
    return (rename(oldName, newName)==0);
}

static void traceUpLevelPath(CBufferT *path) {
    int i;
    if(!path || path->usedLen == 0)
        return;
    i = path->usedLen - 1;
    while(i) {
        if(path->buffer[i] == '/') {
            path->buffer[i] = 0;
            path->usedLen = i;
            break;
        }
        --i;
    }
}


#define DIR_STACK_SIZE 1024 /*2011.01.27: jonezheng for 32 can't satisfy our case,so Imoidfy  32  to 1024.*/
bool cIORemoveDirPort(const char *pathName) {
    DirHandleT dir = 0, stack[DIR_STACK_SIZE];
    DirentT node;
    UInt16 stackPtr = 0, t;
    CBufferT path, file;
    bool ret = false;

    if(pathName == NULL)
        return false;
    memset(&path, 0, sizeof(path));
    memset(&file, 0, sizeof(file));
    if(!cBufAppendString(&path, pathName))
        return false;
    dir = cIOOpenDirPort(path.buffer, NULL);
    if(dir == 0)
        goto exit;
    stack[0] = dir;
    stackPtr = 1;

    while(true) {
        if(!cIOReadDirPort(dir, &node))
            goto next;
        if(DIRENT_isDirectory(node)) {
            if(strcmp(DIRENT_name(node), ".") == 0 || strcmp(DIRENT_name(node), "..") == 0)
                continue;
             if(stackPtr >= DIR_STACK_SIZE) //stack overflow
                continue;
            if(!cBufAppendString(&path, "/"))
                goto exit;
            if(!cBufAppendString(&path, DIRENT_name(node)))
                goto exit;
            if((dir = stack[stackPtr] = cIOOpenDirPort(path.buffer, NULL)) == 0)
                goto next;
            ++stackPtr;
        }
        else if(DIRENT_isRegular(node)) {
            if(cBufFormatString(&file, "%s/%s", path.buffer, DIRENT_name(node)) < 0)
                goto exit;
            cIORemovePort(file.buffer);
        }
        continue;

next:
        cIOCloseDirPort(dir);
        --stackPtr;
        rmdir(path.buffer);
        if(stackPtr == 0) {
            ret = true;
            break;
        }
        traceUpLevelPath(&path);
        dir = stack[stackPtr-1];
    }

exit:
    for(t=0; t<stackPtr; ++t)
        cIOCloseDirPort(stack[t]);
    cBufFree(&path);
    cBufFree(&file);
    return ret;
}

bool cIOTruncatePort(Int32 fd, UInt32 size) {
    return ftruncate(fd, size) == 0;
}


Int32 cIOFsyncPort(Int32 fd)
{
//LEMUR_PROJECT    start
/*<2010.01.21 add by jonezheng for compile windows lexgen.exe.*/
#ifdef HAVE_MS_C_RUNTIME
    return 0;
#else //HAVE_MS_C_RUNTIME
    return fsync(fd);
#endif//HAVE_MS_C_RUNTIME
/*>2010.01.21 add by jonezheng for compile windows lexgen.exe.*/
//LEMUR_PROJECT    end
}

Int32 cIOGetGranularityPort()
{
    return getpagesize();
}

Int32 cIOIsCaseSensitivePort()
{
    return 1;
}

void cIOGetCanonPort(char *path)
{
}

bool cIOSyncPort(Int32 fd)
{
    int ret = -1;
 //LEMUR_PROJECT    start
/*<2010.01.21 add by jonezheng for compile windows lexgen.exe.*/
#ifdef HAVE_MS_C_RUNTIME
#else //HAVE_MS_C_RUNTIME
    ret = fsync(fd);
#endif//HAVE_MS_C_RUNTIME
/*>2010.01.21 add by jonezheng for compile windows lexgen.exe.*/
//LEMUR_PROJECT    end
    if (ret)
    {
        return false;
    }
    return true;

}

void cIOSetLastModifiedImpl()
{
}

void cIOSetReadOnlyImpl()
{
}


bool cIOSetLastModifiedPort(const char *filePath,UInt64 time)
{
    return true;//TODO cIOSetLastModifiedPort
}

char cIOSeparatorCharPort()
{
    return '/';
}

char cIOPathSeparatorCharPort()
{
    return ';';
}
