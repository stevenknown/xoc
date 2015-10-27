/*Copyright 2011 Alibaba Group*/
#include "zip/czip.h"
#include "zip/cziputils.h"
#include "io/cio.h"
#include "mem/cmem.h"
#include "str/cstr.h"

static bool _UnZipTraversalFunc(ZipHandle zip, ZipFile member, char *zippath)
{
    char buffer[256];
    char* p;
    UInt32 len;
    BYTE* data;
    UInt16 pathlen = 0;
    char* path = NULL;

    memset(buffer, 0, sizeof(buffer));
    len = cStrlen(zippath);
    cMemcpy(buffer, zippath, len);
    p = buffer + len;
    *p = '/';
    ++p;

    path = (char*)cZipGetFileName(zip, member, &pathlen);

    if(NULL == path || pathlen == 0) {
        return false;
    }

    cMemcpy(p, path, pathlen);

    if( 0 == strcmp(path, "/"))
        return true;

    if(cZipIsDirectory(zip, member)) {
        cIOMakeDir(buffer);
    }
    else {
        len = cZipGetFileSize(zip, member);
        data = cMalloc(len);
        if(data) {
            Int32 writefd;

            cZipGetFileData(zip, member, data, len, NULL);
            writefd = cIOOpen(buffer, MADA_OF_WRITE | MADA_OF_CREATE | MADA_OF_TRUNC);
            if(writefd < 0) {
                cFree(data);
                return false;
            }
            cIOWrite(writefd, data, len);
            cIOClose(writefd);
            cFree(data);
        }
    }

    return true;
}

bool cZipUnpack(ZipHandle zip, const char* path)
{
    if(!cIOMakeDir(path)) {
        return false;
    }

    return cZipTraversal(zip, (ZipTraversalFunc)_UnZipTraversalFunc, (void*)path);
}

bool cZipUnpackFile(const char* zipFname, const char* targetPath)
{

    ZipHandle zip;
    zip = cZipOpen(zipFname, 0);
    if(!zip)
        return false;

    if(!cZipUnpack(zip, targetPath)) {
        cZipDestroy(zip);
        return false;
    }
    cZipDestroy(zip);
    return true;
}

static bool _DirTraversalFunc(const char *aRootDirName, const char *aSubName, bool isDir, void* aArgs)
{
    ZipHandle zip = (ZipHandle)aArgs;
    UInt16 rootNameLen = 0;
    UInt16 itemNameLen = 0;
    const char *relPath = NULL;
    bool result = false;
    Int32 fd = 0;
    Int32 len = 0;
    void * data = NULL;

    if(isDir)
        return true;

    if(NULL == aRootDirName || NULL == aSubName || NULL == aArgs) {
        return false;
    }

    rootNameLen = cStrlen(aRootDirName);
    itemNameLen = cStrlen(aSubName);
    if(itemNameLen < rootNameLen) {
        return false;
    }

    relPath = aSubName + rootNameLen;
    if('/' == *relPath) {
        relPath++;
    }

    fd = cIOOpen(aSubName, MADA_OF_READ);
    if(fd < 0) {
        printf("105:open file failed: relPath=%s, aSubName=%s\n", relPath, aSubName);
        return false;
    }

    len = cIOSize(fd);
    if(len<=0) {
        cIOClose(fd);
        return false;
    }
    data = cMalloc(len);
    if(!data) {
        goto bail;
    }
    if(cIORead(fd, data, len)<=0)
        goto bail;

    if(!cZipAddFile(zip, relPath, cStrlen(relPath), 0/*Zip_Comp_Deflated*/))
        goto bail;

    if(!cZipAddFileData(zip, data, len))
        goto bail;

    cZipAddFileEnd(zip);

    result = true;

bail:
    if(fd)
        cIOClose(fd);
    if(data)
        cFree(data);

    return result;
}

bool cZipPackDir(const char* zipFname, const char* aSrcPath)
{
    bool isOK = false;

    ZipHandle zip = cZipCreate(100*1024*1024);
    if(!zip) {
        printf("zip create fail!\n");
        return false;
    }
    if(!cIOTraversalDir(aSrcPath, MDFS_ATTR_ARCHIVE, (DirTraversalFuncT)_DirTraversalFunc, (void *)zip)){
        goto bail;
    }

    if(!cZipWrite(zip, zipFname)){
        goto bail;
    }


    isOK = true;

bail:
    if(zip)
        cZipDestroy(zip);
    return isOK;
}
