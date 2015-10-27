/*Copyright 2011 Alibaba Group*/
/*
 *  cio.c
 *  madagascar
 *
 *  Created by Misa.Z on 2/23/09.
 *  Copyright 2009 Virtualnology. All rights reserved.
 *
 */

#include "io/cio.h"
#include "io/cioport.h"
#include "str/cstr.h"
#include "utils/cbuf.h"
#include "mem/cmem.h"

Int32 cIOOpen(const char* fname, UInt32 flags) {
    return cIOOpenPort(fname, flags);
}

void cIOClose(Int32 fd) {
    cIOClosePort(fd);
}

Int32 cIORead(Int32 fd, void* buf, UInt32 bufSize) {
    return cIOReadPort(fd, buf, bufSize);
}

Int32 cIOReadLine(Int32 fd, BYTE* buf, UInt32 size) {
    Int32 ret;

    for(ret=0; ret<(Int32)(size-1); ++ret) {
        BYTE c;
        int len;

        len = cIORead(fd, &c, 1);
        if(len == 0)
            break;
        else if(len < 0)
            return -1;
        else if(c == (BYTE)'\n')
            break;

        buf[ret] = c;
    }

    buf[ret] = 0;
    return ret;
}

Int32 cIOWrite(Int32 fd, const void* buf, UInt32 bufSize) {
    return cIOWritePort(fd, buf, bufSize);
}

Int32 cIOSeek(Int32 fd, Int32 offset, UInt8 flags) {
    return cIOSeekPort(fd, offset, flags);
}

bool cIOGetFileInfo(const char* fname, FileInfoT* info) {
    return cIOGetFileInfoPort(fname, info);
}

bool cIOGetFDInfo(Int32 fd, FileInfoT* info) {
    return cIOGetFDInfoPort(fd, info);
}

bool cIOSetFileAttributes(const char* fname, UInt32 attrs) {
    return cIOSetFileAttributesPort(fname, attrs);
}

bool cIORemove(const char* fname) {
    return cIORemovePort(fname);
}

Int32 cIOSize(Int32 fd) {
    return cIOSizePort(fd);
}

bool cIOTruncate(Int32 fd, UInt32 size) {
    return cIOTruncatePort(fd, size);
}

DirHandleT cIOOpenDir(const char* path, const char* matchPattern) {
    return cIOOpenDirPort(path, matchPattern);
}

bool cIOReadDir(DirHandleT dir, DirentT* info) {
    return cIOReadDirPort(dir, info);
}

void cIORewindDir(DirHandleT dir) {
    cIORewindDirPort(dir);
}

void cIOCloseDir(DirHandleT dir) {
    cIOCloseDirPort(dir);
}

bool cIOMakeDir(const char* path) {
    return cIOMakeDirPort(path);
}

bool cIORemoveDir(const char *pathName) {
    return cIORemoveDirPort(pathName);
}

bool cIOExists(const char* fname) {
    return cIOExistsPort(fname);
}

bool cIORename(const char*oldName,const char* newName) {
    return cIORenamePort(oldName, newName);
}

const char* cIOSafeName(const char* path) {
    static char ret[MADA_MAX_PATH_LEN];
    Int32 i, len;

    len = cStrlen(path);

    for(i=0; i<len && i<MADA_MAX_PATH_LEN; ++i) {
        char c = path[i];
        if((c >= 'a' && c <= 'z') ||
                (c >= 'A' && c <= 'Z') ||
                (c >= '0' && c <= '9') ||
                c == '_' || c == '.')
            ret[i] = c;
        else
            ret[i] = '_';
    }
    ret[i] = 0;
    return ret;
}

//TODO: common implement cIOCopy and cIOMove, port later

bool cIOCopy(const char *aDstFile, const char *aSrcFile)
{
    Int32 srcFd = -1;
    Int32 dstFd = -1;
    Int32 fileSize = -1;
    BYTE *buf = NULL;
    bool isOK = false;

    srcFd = cIOOpen(aSrcFile, MADA_OF_READ);
    if(srcFd <= 0) {
        goto done;
    }

    fileSize = cIOSize(srcFd);
    if(fileSize < 0) {
        goto done;
    }

    buf = cMalloc(fileSize);
    if(NULL == buf) {
        goto done;
    }

    if(cIORead(srcFd, buf, fileSize) != fileSize) {
        goto done;
    }

    dstFd = cIOOpen(aDstFile, MADA_OF_CREATE | MADA_OF_WRITE | MADA_OF_TRUNC);
    if(dstFd <= 0) {
        goto done;
    }

    if(cIOWrite(dstFd, buf, fileSize) != fileSize) {
        goto done;
    }
    isOK = true;
done:
    if(NULL != buf) {
        cFree(buf);
    }
    if(srcFd > 0) {
        cIOClose(srcFd);
    }
    if(dstFd > 0) {
        cIOClose(dstFd);
    }
    return isOK;
}

bool cIOMove(const char *aDstFile, const char *aSrcFile)
{
    if(!cIOCopy(aDstFile, aSrcFile)) {
        return false;
    }
    if(!cIORemove(aSrcFile)) {
        return false;
    }
    return true;
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
bool cIOTraversalDir(const char *aRootDirName, UInt32 aAttr, DirTraversalFuncT  aCallBack, void* aArgs)
{
    //TODO filter

    char srcPath[256];
    char* psrc;
    Int32 len;
    DirHandleT dir;
    DirentT dirent;
    DirHandleT dirStack[DIR_STACK_SIZE];
    UInt32 numStack = 0;
    static const UInt32 bufSize = 4096;
    bool isOK = false;

    len = cStrlen(aRootDirName);
    cMemcpy(srcPath, aRootDirName, len);
    psrc = srcPath + len;
    *psrc = '/';
    ++psrc;

    dir = cIOOpenDir(aRootDirName, NULL);
    if(dir == 0) {
        goto done;
    }
    dirStack[numStack++] = dir;

    while(true) {
readdir:
        if(!cIOReadDir(dir, &dirent))
            goto uplevel;
        if(DIRENT_isDirectory(dirent)) {
            if(cMemcmp(DIRENT_name(dirent), ".", 2) == 0)
                continue;
            if(cMemcmp(DIRENT_name(dirent), "..", 3) == 0)
                continue;
            len = cStrlen(DIRENT_name(dirent));
            cMemcpy(psrc, DIRENT_name(dirent), len);
            psrc += len;
            *psrc = '/';
            ++psrc;
            *psrc = 0;
            dir = cIOOpenDir(srcPath, NULL);
            if(dir == 0) {
                goto done;
            }
            dirStack[numStack++] = dir;

            if(numStack >= DIR_STACK_SIZE) {
                printf("ERROR/ folder Stack overflow,numStack=%d\n",numStack);
                goto done;
            }

            if(MDFS_COPYDIR_FOR_LEXGEN_ATTR == aAttr){
                if(NULL != aCallBack) {
                    if(!aCallBack(aRootDirName, srcPath, true, (void*)aArgs))
                        continue;
                }
            }
        }
        else {
            len = cStrlen(DIRENT_name(dirent));

            cMemcpy(psrc, DIRENT_name(dirent), len);
            psrc[len] = 0;
            if(NULL != aCallBack) {
                if(MDFS_COPYDIR_FOR_LEXGEN_ATTR == aAttr){
                    if(!aCallBack(aRootDirName, srcPath, false, (void*)aArgs))
                        continue;
                }else{
                    if(!aCallBack(aRootDirName, srcPath, false, (void*)aArgs))
                        goto done;
                }
            }
        }
        continue;

uplevel:
        cIOCloseDir(dir);
        len = 0;
        --psrc;
        --numStack;
        while(numStack) {
            --psrc;
            ++len;
            if(*psrc == '/') {
                ++psrc;
                *psrc = 0;
                dir = dirStack[numStack-1];
                goto readdir;
            }
        }
        break;
    }
    isOK = true;

done:
    while(numStack--) {
        cIOCloseDir(dirStack[numStack]);
    }

    return isOK;
}

//** added by raymond
Int32 cIOFsync(Int32 fd)
{
    return cIOFsyncPort(fd);
}

Int32 cIOGetGranularity()
{
    return cIOGetGranularityPort();
}

Int32 cIOIsCaseSensitive()
{
    return cIOIsCaseSensitivePort();
}

void cIOGetCanon(char *path)
{
    cIOGetCanonPort(path);
}

bool cIOSync(Int32 fd)
{
    return cIOSyncPort(fd);
}

//**

bool cIOSetLastModified(const char *filePath,UInt64 time)
{
    return cIOSetLastModifiedPort(filePath, time);
}

char cIOSeparatorChar()
{
    return cIOSeparatorCharPort();
}

char cIOPathSeparatorChar()
{
    return cIOPathSeparatorCharPort();
}


