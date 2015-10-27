/*Copyright 2011 Alibaba Group*/
/*
 *  cIOport.c
 *  madagascar
 *
 *  Created by Misa.Z on 2/22/09.
 *  Copyright 2009 Virtualnology. All rights reserved.
 *
 */

#include "io/cioport.h"
#include "errno/cerrno.h"
#include <fcntl.h>
#include <sys/stat.h>
#include <io.h>
#include <stdio.h>
#include <direct.h>
#include <stdlib.h>

#include <Windows.h>

#include "mem/cmemport.h"
#include "utils/cbuf.h"

#define DIR_STACK_SIZE 32
#define PATH_MAX_LENGTH   256

extern UInt32 UTF8ToANSI(const char* src, UInt32 cbSrc, char* dst, UInt32 cbDst);
extern UInt32 ANSIToUTF8(const char* src, UInt32 cbSrc, char* dst, UInt32 cbDst);


static UInt32 UTF8ToANSIFileName(const char* src, char* dst, Int32 dstLen) {
    memset(dst, 0, dstLen);
    return UTF8ToANSI(src, strlen(src), dst, dstLen);
}

static UInt32 ANSIToUTF8FileName(const char* str, char* dst, Int32 dstLen) {
    memset(dst, 0, dstLen);
    return ANSIToUTF8(str, strlen(str), dst, dstLen);
}

static const char* UTF8ToANSIFileNameExt(const char* src) {
    static char tempFileName[1024];
    UTF8ToANSI(src, strlen(src), tempFileName, 1024);
    return tempFileName;
}


static UInt32 mapFileAttr(DWORD localAttr) {
    UInt32 mapAttr = 0;
    mapAttr |= (localAttr & FILE_ATTRIBUTE_ARCHIVE)? MDFS_ATTR_ARCHIVE : 0;
    mapAttr |= (localAttr & FILE_ATTRIBUTE_DIRECTORY)? MDFS_ATTR_DIR : 0;
    mapAttr |= MDFS_ATTR_READ; // referrence phoneme.
    mapAttr |= (localAttr & FILE_ATTRIBUTE_READONLY)? 0 : MDFS_ATTR_WRITE;
    mapAttr |= (localAttr & FILE_ATTRIBUTE_HIDDEN)? MDFS_ATTR_HIDDEN : 0;

    return mapAttr;
}

Int32 cIOOpenPort(const char* fname, UInt32 of) {
    Int32 flags = 0;
    char localFileName[PATH_MAX_LENGTH];

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

    UTF8ToANSIFileName(fname, localFileName, sizeof(localFileName));
    if(of & MADA_OF_CREATE)
        return open(localFileName, flags|O_BINARY, 0666);
    else
        return open(localFileName, flags|O_BINARY);
}

void cIOClosePort(Int32 fd) {
    if(fd >= 0)
        close(fd);
}

Int32 cIOReadPort(Int32 fd, void* buf, UInt32 bufSize) {
    if(fd < 0 || buf == NULL)
        return -1;

    return read(fd, buf, bufSize);
}

Int32 cIOWritePort(Int32 fd, const void* buf, UInt32 bufSize) {
    if(fd < 0 || buf == NULL)
        return -1;
    return write(fd, buf, bufSize);
}

Int32 cIOSeekPort(Int32 fd, Int32 offset, UInt8 sf) {
    Int32 flags = SEEK_SET;
    if(fd < 0)
        return -1;

    if(sf == SK_SET)
        flags = SEEK_SET;
    else if(sf == SK_CUR)
        flags = SEEK_CUR;
    else if(sf == SK_END)
        flags = SEEK_END;


    return lseek(fd, offset, flags);
}

Int32 cIOSizePort(int fd) {
    struct stat st;

    if (fstat(fd, &st) != 0) {
        return -1;
    }

    return st.st_size;
}

#ifdef _MSC_VER
#define    S_ISDIR(m)    (((m) & S_IFMT) == S_IFDIR)
#define    S_ISFIFO(m)    (((m) & S_IFMT) == S_IFIFO)
#define    S_ISCHR(m)    (((m) & S_IFMT) == S_IFCHR)
#define    S_ISBLK(m)    (((m) & S_IFMT) == S_IFBLK)
#define    S_ISREG(m)    (((m) & S_IFMT) == S_IFREG)
#endif

static UInt32 mapAttributes(UInt16 mode) {
    UInt32 attrs = 0;

    if (S_ISDIR(mode)) {
        attrs |= MDFS_ATTR_DIR;
    }

    if (S_ISREG(mode)) {
        attrs |= MDFS_ATTR_ARCHIVE;
    }

    if ((mode & S_IREAD)) {
        attrs |= MDFS_ATTR_READ;
    }

    if ((mode & S_IWRITE)) {
        attrs |= MDFS_ATTR_WRITE;
    }

    return attrs;
}

bool cIOGetFileInfoPort(const char* fname, FileInfoT* info) {
    char localFileName[PATH_MAX_LENGTH];

    struct stat st;
    if(fname == NULL || info == NULL) {
        _cerrno = ERROR_MEM_NULL_OBJ;
        return false;
    }

    UTF8ToANSIFileName(fname, localFileName, sizeof(localFileName));
    if(stat(localFileName, &st) != 0)
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

bool cIOExistsPort(const char* fname) {
    char localFileName[PATH_MAX_LENGTH];

    if(fname == NULL || fname[0] == 0)
        return false;
    UTF8ToANSIFileName(fname, localFileName, sizeof(localFileName));
    return (access(localFileName, O_RDONLY) != -1);
}

bool cIORemovePort(const char* fname) {
    char localFileName[PATH_MAX_LENGTH];

    if(fname == NULL || fname[0] == 0)
        return false;
    UTF8ToANSIFileName(fname, localFileName, sizeof(localFileName));
    return (remove(localFileName) == 0);
}

bool cIORenamePort(const char*oldName,const char* newName) {
    char localOldName[PATH_MAX_LENGTH];
    char localNewName[PATH_MAX_LENGTH];

    if(oldName == NULL || newName == NULL)
        return false;

    UTF8ToANSIFileName(oldName, localOldName, sizeof(localOldName));
    UTF8ToANSIFileName(newName, localNewName, sizeof(localNewName));
    return (rename(localOldName, localNewName)==0);
}

bool cIOMakeDirPort(const char* path) {
    char *tmp;
    int sz, i = 0;
    char localPathName[PATH_MAX_LENGTH];

    if(path == NULL || path[0] == 0)
        return false;
    sz = strlen(path)+1;
    tmp = malloc(sz);
    if(!tmp)
        return false;
    memset(tmp, 0, sz);
    while(path[i] != 0) {
        if(path[i] == '/') {
            memcpy(tmp, path, i+1);
            if(!cIOExistsPort(tmp)) {
                UTF8ToANSIFileName(tmp, localPathName, sizeof(localPathName));
                if(_mkdir(localPathName) != 0)
                    goto failed;
            }
        }
        ++i;
    }
    if(!cIOExistsPort(path)) {
        UTF8ToANSIFileName(path, localPathName, sizeof(localPathName));
        if(_mkdir(localPathName) != 0)
            goto failed;
    }
    free(tmp);
    return true;

failed:
    cFreePort(tmp);
    return false;
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

bool cIORemoveDirPort(const char *pathName) {
    DirHandleT dir = 0, stack[DIR_STACK_SIZE];
    DirentT node;
    UInt16 stackPtr = 0, t;
    CBufferT path, file;
    bool ret = false;
    char localPathName[PATH_MAX_LENGTH];

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
        if(!cIOReadDirPort(dir,&node))
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
        UTF8ToANSIFileName(path.buffer, localPathName, sizeof(localPathName));
        _rmdir(localPathName);
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

typedef struct DirectoryInfoStruct {
    HANDLE dirHandle;
    int first;
    WIN32_FIND_DATA dirent;
    struct DirectoryInfoStruct *next;
}DirectoryInfo;
static DirectoryInfo *dirInfo = NULL;

bool addDirectoryInfo(HANDLE dir, WIN32_FIND_DATA *dirent) {
    DirectoryInfo *tmp;

    tmp = (DirectoryInfo*)malloc(sizeof(DirectoryInfo));
    if(!tmp)
        return false;
    tmp->dirHandle = dir;
    tmp->first = 1;
    memcpy(&tmp->dirent, dirent, sizeof(WIN32_FIND_DATA));
    tmp->next = dirInfo;
    dirInfo = tmp;
    return true;
}

DirectoryInfo *getDirectoryInfo(HANDLE dir) {
    DirectoryInfo *p;

    p = dirInfo;
    while(p) {
        if(p->dirHandle == dir)
            return p;
        p = p->next;
    }
    return NULL;
}

bool eraseDirectoryInfo(HANDLE dir) {
    DirectoryInfo *p, *pre = NULL;

    p = dirInfo;
    while(p) {
        if(p->dirHandle == dir) {
            if(pre)
                pre->next = p->next;
            else
                dirInfo = p->next;
            free(p);
            return true;
        }
        pre = p;
        p = p->next;
    }
    return false;
}

DirHandleT cIOOpenDirPort(const char* path, const char* matchPattern) {
    HANDLE h;
    WIN32_FIND_DATA tmp;
    char *findStr;
    char localPathName[PATH_MAX_LENGTH];

    if(!path)
        return (DirHandleT)0;
    findStr = (char*)malloc(strlen(path)+3);
    if(!findStr)
        return (DirHandleT)0;
    sprintf(findStr, "%s/*", path);
    UTF8ToANSIFileName(findStr, localPathName, sizeof(localPathName));
    h = FindFirstFile(localPathName, &tmp);
    free(findStr);
    if(h == INVALID_HANDLE_VALUE)
        return 0;
    if(!addDirectoryInfo(h, &tmp))
        return 0;
    return (DirHandleT)h;
}

/**
Description:
@return: 0- fail; >0 -ok; -1 -the end file of this directory
*/
bool cIOReadDirPort(DirHandleT dir, DirentT *pDirentInfo) {

    DirectoryInfo *p;
    if(pDirentInfo == NULL)
        return false;

    p = getDirectoryInfo((HANDLE)dir);
    if(!p)
        return false;
    if(p->first) {
        p->first = 0;
        //return (Dirent)&p->dirent;
        goto Correct;
    }
    if(!FindNextFile((HANDLE)dir, &p->dirent))
        return false;

Correct:
    ANSIToUTF8FileName(p->dirent.cFileName, pDirentInfo->fileName, sizeof(pDirentInfo->fileName));
    pDirentInfo->fileAttributes = mapFileAttr(p->dirent.dwFileAttributes);
    return true;
}

void cIORewindDirPort(DirHandleT dir) {
}

void cIOCloseDirPort(DirHandleT dir) {
    FindClose((void*)dir);
    eraseDirectoryInfo((HANDLE)dir);
}

bool cIOTruncatePort(Int32 fd, UInt32 size) {
    return _chsize(fd, size) == 0;
}

//** added by raymond
Int32 cIOFsyncPort(Int32 fd)
{
    return FlushFileBuffers((HANDLE) fd);
}

Int32 cIOGetGranularityPort()
{
    SYSTEM_INFO info;
    GetSystemInfo(&info);
    return info.dwAllocationGranularity;
}

Int32 cIOIsCaseSensitivePort()
{
    return 1;
}

static Int32 platformFindfirst (char *path, char *resultbuf)
{
  /*
   * Takes a path and a preallocated resultbuf.  Answers a handle to be used
   * in subsequent calls to hyfile_findnext and hyfile_findclose.  Handle may
   * be -1 if hyfile_findfirst fails.
   * The parameter @path will only be a valid directory name, any info that must
   * be added to it and passed to the os (c:\\) should be (c:\\*) for Win32
   */
  WIN32_FIND_DATA lpFindFileData;
  HANDLE handle;

  handle = FindFirstFile ((LPCTSTR) path, &lpFindFileData);

  if (handle == INVALID_HANDLE_VALUE)
    return (Int32) - 1;

  lstrcpy (resultbuf, lpFindFileData.cFileName);
  FindClose (handle);
  return 1;
}


void cIOGetCanonPort(char *path)
{
    Int32 result;
    Int32 pos, start, length, rpos, rlen;
    char newpath[MADA_MAX_PATH_LEN];
    char filename[MADA_MAX_PATH_LEN];

  pos = 0;
  length = strlen (path);
  if (length >= 2 && path[1] == ':')
    {
      path[0] = toupper (path[0]);
      pos = 2;
    }
  else if (path[0] == '\\')
    {
      pos++;
      if (length >= 2 && path[1] == '\\')
        {
          pos++;
          /* Found network path, skip server and volume */
          while (pos < length && path[pos] != '\\')
            pos++;
          if (path[pos] == '\\')
            pos++;
          while (pos < length && path[pos] != '\\')
            pos++;
          if (pos == length)
            return;
        }
    }
  if (path[pos] == '\\')
    pos++;
  start = pos;
  memcpy (newpath, path, pos);
  rpos = pos;
  while (pos <= length)
    {
      if (path[pos] == '\\' || pos == length)
        {
          if (pos == length && path[pos - 1] == '\\')
            break;
          path[pos] = 0;
          result = platformFindfirst (path, filename);
          if (pos != length)
            path[pos] = '\\';
          if (result == ((Int32) - 1))
            {
              break;
            }
          else
            {
              rlen = strlen (filename);
              if (rpos + rlen + 2 >= MADA_MAX_PATH_LEN)
                break;
              strcpy (&newpath[rpos], filename);
              rpos += rlen;
              if (pos != length)
                newpath[rpos++] = '\\';
              else
                newpath[rpos] = 0;
              start = pos + 1;
            }
        }
      else if (path[pos] == '*' || path[pos] == '?')
                  break;
      pos++;
    }
  if (start <= length)
    strncpy (&newpath[rpos], &path[start], MADA_MAX_PATH_LEN - 1 - rpos);
  strcpy (path, newpath);
}


bool cIOSyncPort(Int32 fd)
{
    return FlushFileBuffers((HANDLE) fd);
}

//** end

bool cIOSetLastModifiedPort(const char *filePath,UInt64 time)
{
    return true;//TODO cIOSetLastModifiedPort
}

char cIOSeparatorCharPort()
{
    return '\\';
}

char cIOPathSeparatorCharPort()
{
    return ';';
}
