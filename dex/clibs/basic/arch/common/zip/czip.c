/*Copyright 2011 Alibaba Group*/
#include "zip/czip.h"
#include "zip/czipport.h"
#include "utils/chashtable.h"
#include "io/cio.h"
#include "str/cstr.h"
#include "utils/clbe.h"
#include "mem/cmem.h"
#include "utils/ccrc32.h"
#include <unistd.h>
#include <errno.h>
#include "trace/ctrace.h"

typedef struct ZipItemStruct ZipItem;

struct ZipItemStruct {
    ZipItem* parent;
    ZipItem* prev;
    ZipItem* next;
    UInt16 flags;
    UInt16 nameLen;
    char* name;
    /** this item is directory: child zip item pointer.
     *  this item is file: file data offset
     */
    ULong off_childs; //UInt32 off_childs;
    UInt32 uncompSize;
    UInt32 compSize;
    UInt16 compMode;
    UInt16 reserved;
    UInt32 offsetInZipFile;
    UInt32 compID;
    UInt32 crc32;
    UInt32 specSize;
};

typedef struct {
    ZipItem* root;
    HashTableHandle allItems;
    UInt32 numFiles;
    Int32 zipfd;
    BYTE* compBuffer;
    UInt32 compBufSize;
    UInt32 curCompHeader;
    ZipItem* newFile;
    Int32 tmpFileDes;
    UInt32 tmpFileSize;
}ZipInfo;

#define Flags_ZipItem_Directory                                    0x8000
#define Flags_ZipItem_tmpFile                                    0x0001
#define MIN_COMPRESS_BUFFER_SIZE                                1024
#define ZIP_TMP_PATH                                            ".czip_tmp"
#define ZIP_CENTRAL_DIR_END_SIZE                                22
#define ZIP_LOCAL_FILE_HEADER_SIZE                                30

#define zbufAlloc cMalloc
#define zbufFree cFree

static const UInt32 localFileHeaderSignatrue = 0x04034b50;
static const UInt32 centralFileHeaderSignature = 0x02014b50;
static const UInt32 centralDirEndSignature = 0x06054b50;

typedef bool (*ZipItemHandler)(ZipItem* it, void* args);


static char* getShortNameAndPath(const char* longName, UInt16 nameLen, char** path) {
    UInt16 i = nameLen;
    char* shortName;

    if(longName[nameLen - 1] == '/')
        --i;
    while(i) {
        --i;
        if(longName[i] == '/')
            break;
    }
    if(longName[0] == '/')
        return NULL;
    if(i == 0) {
        *path = NULL;
        shortName = (char*)cMalloc(nameLen + 1);
        if(shortName == NULL)
            return NULL;
        cMemcpy(shortName, longName, nameLen);
        shortName[nameLen] = 0;
        return shortName;
    }
    shortName = (char*)cMalloc(nameLen + 2);
    if(shortName == NULL)
        return NULL;
    cMemcpy(shortName, longName + i + 1, nameLen - i - 1);
    shortName[nameLen - i - 1] = 0;
    *path = shortName + nameLen - i;
    cMemcpy(*path, longName, i + 1);
    (*path)[i+1] = 0;
    return shortName;
}

static bool isValidPathName(const char* name, UInt16 len) {
    return true;
}

static bool isValidName(const char* name, UInt16 len) {
    return name[len - 1] != '/';
}

ZipHandle cZipCreate(UInt32 bufSize) {
    UInt32 sz;
    ZipInfo* zip;

    if(bufSize < MIN_COMPRESS_BUFFER_SIZE)
        bufSize = MIN_COMPRESS_BUFFER_SIZE;
    sz = sizeof(ZipInfo) + sizeof(ZipItem) + 2 + bufSize;
    zip = (ZipInfo*)cMalloc(sz);
    if(zip == NULL)
        return 0;
    cMemset(zip, 0, sz);
    zip->root = (ZipItem*)(zip + 1);
    zip->root->flags |= Flags_ZipItem_Directory;
    zip->root->nameLen = 1;
    zip->root->name = (char*)(zip->root + 1);
    zip->root->name[0] = '/';

    zip->allItems = cHashCreate(NULL, 16, 0.0, 0);
    if(zip->allItems == 0) {
        cFree(zip);
        return 0;
    }
    if(!cHashInsert(zip->allItems, zip->root->name, zip->root->nameLen, zip->root, 0)) {
        cHashFree(zip->allItems);
        cFree(zip);
        return 0;
    }

    zip->compBuffer = (BYTE*)(zip->root + 1);
    zip->compBuffer += 2;
    zip->compBufSize = bufSize;
    zip->curCompHeader = 0;
    *(UInt32*)zip->compBuffer = 0;
    zip->zipfd = -1;
    zip->tmpFileDes = -1;
    return (ZipHandle)zip;
}


ZipHandle cZipCreateByMemory(UInt32 bufSize,void* bufferAddr) {
    UInt32 sz;
    ZipInfo* zip;

    //Use_static_memory
    #if 0
    if(bufSize < MIN_COMPRESS_BUFFER_SIZE)
        bufSize = MIN_COMPRESS_BUFFER_SIZE;

    sz = sizeof(ZipInfo) + sizeof(ZipItem) + 2 + bufSize;
    zip = (ZipInfo*)cMalloc(sz);
    #else
    LOGD("cZipCreateByMemory bufferAddr=0x%x",(UInt32)(ULong)bufferAddr);
    sz = bufSize;
    zip = (ZipInfo*)bufferAddr;
    #endif
    if(zip == NULL)
        return 0;
    cMemset(zip, 0, sz);
    zip->root = (ZipItem*)(zip + 1);
    zip->root->flags |= Flags_ZipItem_Directory;
    zip->root->nameLen = 1;
    zip->root->name = (char*)(zip->root + 1);
    zip->root->name[0] = '/';

    zip->allItems = cHashCreate(NULL, 16, 0.0, 0);
    if(zip->allItems == 0) {
        //cFree(zip);
        return 0;
    }
    if(!cHashInsert(zip->allItems, zip->root->name, zip->root->nameLen, zip->root, 0)) {
        cHashFree(zip->allItems);
        //cFree(zip);
        return 0;
    }

    zip->compBuffer = (BYTE*)(zip->root + 1);
    zip->compBuffer += 2;
    zip->compBufSize = bufSize -(sizeof(ZipInfo) + sizeof(ZipItem) + 2);
    zip->curCompHeader = 0;
    *(UInt32*)zip->compBuffer = 0;
    zip->zipfd = -1;
    zip->tmpFileDes = -1;
    return (ZipHandle)zip;
}





static ZipItem* getParentZipItem(ZipInfo* zipInfo, const char* name, UInt16 nameLen) {
    char* shortName;
    char* path;
    ZipItem* dir;
    UInt16 shortNameLen;

    shortName = getShortNameAndPath(name, nameLen, &path);
    if(shortName == NULL)
        return NULL;
    shortNameLen = (UInt16)cStrlen(shortName);

    if(path) {
        dir = (ZipItem*)cHashGet(zipInfo->allItems, path, cStrlen(path), NULL);
        if(dir == NULL) {
            cFree(shortName);
            return NULL;
        }
    }
    else
        dir = zipInfo->root;
    cFree(shortName);
    return dir;
}

static ZipItem* createZipItem(const char* name, UInt16 nameLen, UInt16 compMode) {
    ZipItem* newFile;
    UInt32 size;

    //create tree node of new file
    size = sizeof(ZipItem) + nameLen + 1;
    newFile = (ZipItem*)cMalloc(size);
    if(newFile == NULL)
        return NULL;
    cMemset(newFile, 0, size);
    newFile->nameLen = nameLen;
    newFile->name = (char*)(newFile + 1);
    cMemcpy(newFile->name, name, nameLen);
//    newFile->crc32 = 0xdebb20e3;
    newFile->crc32 = 0;
    newFile->compMode = compMode;
    return newFile;
}

static bool addZipItem(ZipInfo* zipInfo, ZipItem* item, ZipItem* parent) {
    ZipItem* childs;

    if(!cHashInsert(zipInfo->allItems, item->name, item->nameLen, item, 0))
        return false;
    item->parent = parent;
    item->prev = NULL;
    childs = (ZipItem*)parent->off_childs;
    if(childs)
        childs->prev = item;
    item->next = childs;
    parent->off_childs = (ULong)item;
    return true;
}

static ZipItem* addZipItemRecursive(ZipInfo* zipInfo, const char* name, UInt16 nameLen, UInt16 compMode) {
    const char* p;
    ZipItem* item;
    ZipItem* parent;

    p = name;
    parent = zipInfo->root;
    item = NULL;
    while(p - name < nameLen) {
        if(*p == '/') {
            ++p;
            item = (ZipItem*)cHashGet(zipInfo->allItems, name, p - name, NULL);
            if(item == NULL) {
                item = createZipItem(name, p - name, 0);
                if(item == NULL)
                    return NULL;
                if(!addZipItem(zipInfo, item, parent)) {
                    cFree(item);
                    return NULL;
                }
                item->flags |= Flags_ZipItem_Directory;
            }
            parent = item;
        }
        else
            ++p;
    }
    if(parent->nameLen < nameLen) {
        item = createZipItem(name, nameLen, compMode);
        if(item == NULL)
            return NULL;
        if(!addZipItem(zipInfo, item, parent)) {
            cFree(item);
            return NULL;
        }
    }
    return item;
}

static ZipItem* addNewZipItem(ZipInfo* zipInfo, const char* name, UInt16 nameLen, UInt16 compMode) {
    ZipItem* dir;
    ZipItem* newFile;

    dir = getParentZipItem(zipInfo, name, nameLen);
    if(dir == NULL)
        return addZipItemRecursive(zipInfo, name, nameLen, compMode);
    newFile = createZipItem(name, nameLen, compMode);
    if(newFile == NULL)
        return NULL;
    if(!addZipItem(zipInfo, newFile, dir)) {
        cFree(newFile);
        return NULL;
    }
    return newFile;
}

bool cZipAddFile(ZipHandle zip, const char* name, UInt16 nameLen, UInt16 compMode) {
    ZipInfo* zipInfo;
    ZipItem* newFile;
    ZipItem** p;

    if(zip == 0 || name == NULL || nameLen == 0)
        return false;

    zipInfo = (ZipInfo*)zip;
    //previous adding file process not finish.
    if(zipInfo->newFile)
        return false;
    if(!isValidName(name, nameLen))
        return false;
    //file already exist
    if(cHashGet(zipInfo->allItems, name, nameLen, NULL))
        return false;

    newFile = addNewZipItem(zipInfo, name, nameLen, compMode);
    if(newFile == NULL)
        return false;

    //initialize compression buffer for the new item
    p = (ZipItem**)(zipInfo->compBuffer + zipInfo->curCompHeader);
    newFile->off_childs = zipInfo->curCompHeader;
    *p = newFile;
    zipInfo->curCompHeader += 4;
    zipInfo->newFile = newFile;
    ++zipInfo->numFiles;
    return true;
}

static void getZipTmpFileName(ZipInfo* zipInfo, char* outName, UInt32 outBufSize) {
    cIOMakeDir(ZIP_TMP_PATH);
    cSnprintf(outName, outBufSize, "%s/%u_%x", ZIP_TMP_PATH, getpid(), (UInt32)(ULong)zipInfo);
}

static bool flushCompressBuffer(ZipInfo* zipInfo) {
    char tmpName[64];
    BYTE* p;
    ZipItem* item;

    getZipTmpFileName(zipInfo, tmpName, sizeof(tmpName));
    if(zipInfo->tmpFileDes < 0) {
        zipInfo->tmpFileDes = cIOOpen(tmpName, MADA_OF_CREATE | MADA_OF_WRITE | MADA_OF_READ | MADA_OF_TRUNC);
        if(zipInfo->tmpFileDes < 0)
            return false;
    }

    p = zipInfo->compBuffer;
    while((UInt32)(p - zipInfo->compBuffer) < zipInfo->curCompHeader) {
        item = *(ZipItem**)p;
        p += 4;
        if(item->flags & Flags_ZipItem_tmpFile) {
            UInt32 c;

            c = item->compSize - item->specSize;
            if(c) {
                if(cIOWrite(zipInfo->tmpFileDes, p, c) < 0) {
                    cIOClose(zipInfo->tmpFileDes);
                    zipInfo->tmpFileDes = -1;
                    return false;
                }
            }
            item->specSize += c;
            p += c;
            zipInfo->tmpFileSize += c;
        }
        else {
            if(item->compSize) {
                if(cIOWrite(zipInfo->tmpFileDes, p, item->compSize) < 0) {
                    cIOClose(zipInfo->tmpFileDes);
                    zipInfo->tmpFileDes = -1;
                    return false;
                }
                p += item->compSize;
                item->flags |= Flags_ZipItem_tmpFile;
                item->off_childs = zipInfo->tmpFileSize;
                zipInfo->tmpFileSize += item->compSize;
            }
        }
    }

    p = zipInfo->compBuffer;
    *(ZipItem**)p = zipInfo->newFile;
    zipInfo->curCompHeader = 4;
    return true;
}

bool cZipAddFileData(ZipHandle zip, BYTE* data, UInt32 size) {
    ZipInfo* zipInfo;
    ZipItem* item;
    BYTE* out;
    UInt32 outBufSize;
    UInt32 outSize = 0;;

    if(zip == 0 || data == NULL)
        return false;
    if(size == 0)
        return true;

    zipInfo = (ZipInfo*)zip;
    item = zipInfo->newFile;

    item->crc32 = ccrc32(data, size, item->crc32);

    if(item->compMode == Zip_Comp_Deflated) {
        Int32 inSize = 0;

        if(item->compID == 0) {
            item->compID = cZipCompFlateInit();
            if(item->compID == 0)
                goto failed;
        }

        while(true) {
            out = zipInfo->compBuffer + zipInfo->curCompHeader;
            outBufSize = zipInfo->compBufSize - (out - zipInfo->compBuffer);
            inSize = cZipDeflate(item->compID, data, size, out, outBufSize, &outSize);
            if(inSize < 0) {
                cZipCompFlateEnd(item->compID);
                goto failed;
            }
            item->compSize += outSize;
            if((item->flags & Flags_ZipItem_tmpFile) == 0)
                item->specSize += outSize;
            zipInfo->curCompHeader += outSize;
            item->uncompSize += inSize;
            size -= inSize;
            data += inSize;
            if(size == 0)
                break;
            if(!flushCompressBuffer(zipInfo)) {
                cZipCompFlateEnd(item->compID);
                goto failed;
            }
        }
    }
    else {
        UInt32 cpSize;

        while(true) {
            out = zipInfo->compBuffer + zipInfo->curCompHeader;
            outBufSize = zipInfo->compBufSize - (out - zipInfo->compBuffer);
            if(outBufSize > size)
                cpSize = size;
            else
                cpSize = outBufSize;
            cMemcpy(out, data, cpSize);
            data += cpSize;
            size -= cpSize;
            item->uncompSize += cpSize;
            if((item->flags & Flags_ZipItem_tmpFile) == 0)
                item->specSize += cpSize;
            item->compSize += cpSize;
            zipInfo->curCompHeader += cpSize;
            if(size == 0)
                break;
            if(!flushCompressBuffer(zipInfo))
                goto failed;
        }
    }
    return true;

failed:
    if(item->flags & Flags_ZipItem_tmpFile)
        zipInfo->curCompHeader = 0;
    else
        zipInfo->curCompHeader -= item->compSize;
    cFree(zipInfo->newFile);
    zipInfo->newFile = NULL;
    return false;
}

void cZipAddFileEnd(ZipHandle zip) {
    ZipInfo* zipInfo;
    BYTE* out;
    UInt32 outBufSize;
    UInt32 outSize;
    Int32 ret;
    ZipItem* item;

    if(zip == 0)
        return;
    zipInfo = (ZipInfo*)zip;
    if(zipInfo->newFile == NULL)
        return;
    item = zipInfo->newFile;
    if(item->compMode == Zip_Comp_Deflated) {
        while(true) {
            out = zipInfo->compBuffer + zipInfo->curCompHeader;
            outBufSize = zipInfo->compBufSize - (out - zipInfo->compBuffer);
            ret = cZipDeflateFinish(item->compID, out, outBufSize, &outSize);
            if(ret < 0)
                goto failed;
            item->compSize += outSize;
            if((item->flags & Flags_ZipItem_tmpFile) == 0)
                item->specSize += outSize;
            zipInfo->curCompHeader += outSize;
            if(ret == 0)
                break;
            if(!flushCompressBuffer(zipInfo))
                goto failed2;
        }
    }

    /**
    if(zipInfo->compBufSize - zipInfo->curCompHeader < 100) {
        if(!flushCompressBuffer(zipInfo)) {
            zipInfo->curCompHeader = 0;
            head = (UInt32*)zipInfo->compBuffer;
            head[0] = 0;
            head[1] = 0;
        }
    }
    */
    if(zipInfo->newFile->compID) {
        cZipCompFlateEnd(zipInfo->newFile->compID);
        zipInfo->newFile->compID = 0;
    }
    zipInfo->newFile = NULL;

failed:
    //TODO: remove the file item
    //clear compress buffer

    //avoid compile error
    zipInfo->newFile = NULL;

failed2:
    //TODO:  clear compress buffer
    //other????

    //avoid compile error
    zipInfo->newFile = NULL;
}

bool cZipAddDirectory(ZipHandle zip, const char* path, UInt16 pathLen) {
    ZipInfo* zipInfo;
    ZipItem* newDir;
    char* dirPath = NULL;
    const char* cpath;
    /**
    char* parDir;
    char* shortName;
    ZipItem* dir;
    ZipItem* childs;
    */

    if(zip == 0 || path == NULL || pathLen == 0)
        return false;
    zipInfo = (ZipInfo*)zip;
    if(path[pathLen - 1] == '/')
        cpath = path;
    else {
        ++pathLen;
        dirPath = (char*)cMalloc(pathLen);
        if(dirPath == NULL)
            return false;
        cMemcpy(dirPath, path, pathLen - 1);
        dirPath[pathLen - 1] = '/';
        cpath = dirPath;
    }
    newDir = addZipItemRecursive(zipInfo, cpath, pathLen, 0);
    if(newDir == NULL)
        return false;
    newDir->flags |= Flags_ZipItem_Directory;
    if(dirPath)
        cFree(dirPath);
    /**
    shortName = getShortNameAndPath(path, pathLen, &parDir);
    if(shortName == NULL)
        return false;
    zipInfo = (ZipInfo*)zip;
    if(parDir) {
        dir = (ZipItem*)cHashGet(zipInfo->allItems, parDir, cStrlen(parDir), NULL);
        if(dir == NULL) {
            cFree(shortName);
            return false;
        }
    }
    else
        dir = zipInfo->root;
    newDir = (ZipItem*)cMalloc(sizeof(ZipItem) + pathLen + 2);
    if(newDir == NULL) {
        cFree(shortName);
        return false;
    }
    cMemset(newDir, 0, sizeof(ZipItem));
    newDir->name = (char*)(newDir + 1);
    newDir->nameLen = pathLen;
    cMemcpy(newDir->name, path, pathLen);
    if(path[pathLen - 1] != '/') {
        newDir->name[pathLen] = '/';
        newDir->name[pathLen + 1] = 0;
        ++newDir->nameLen;
    }
    else
        newDir->name[pathLen] = 0;
    newDir->flags |= Flags_ZipItem_Directory;
    if(!cHashInsert(zipInfo->allItems, newDir->name, newDir->nameLen, newDir, 0)) {
        cFree(shortName);
        cFree(newDir);
        return false;
    }
    newDir->parent = dir;
    newDir->prev = NULL;
    childs = (ZipItem*)dir->off_childs;
    newDir->next = childs;
    if(childs)
        childs->prev = newDir;
    dir->off_childs = (UInt32)newDir;
    cFree(shortName);
    */
    return true;
}

bool cZipRemove(ZipHandle zip, const char* pathName, UInt16 nameLen) {
    ZipInfo* zipInfo;
    ZipItem* item;
    ZipItem* parent;

    if(zip == 0 || pathName == NULL || nameLen == 0)
        return false;

    zipInfo = (ZipInfo*)zip;
    item = (ZipItem*)cHashGet(zipInfo->allItems, pathName, nameLen, NULL);
    if(item == NULL)
        return false;
    parent = item->parent;
    //can't remove root directory
    if(parent == NULL)
        return false;
    if(item->flags & Flags_ZipItem_Directory) {
        //can't remove not empty directory
        if(item->off_childs)
            return false;
    }
    if(item->prev == NULL)
        parent->off_childs = (ULong)item->next;
    else
        item->prev->next = item->next;
    if(item->next)
        item->next->prev = item->prev;
    cHashRemove(zipInfo->allItems, pathName, nameLen);

    if((item->flags & Flags_ZipItem_Directory) == 0) {
        zbufFree((BYTE*)item->off_childs);
        --zipInfo->numFiles;
    }
    cFree(item);
    return true;
}

#define FILE_COPY_BUFFER_SIZE            8192
static bool copyDataInFile(Int32 from, Int32 to, UInt32 offset, UInt32 size) {
    BYTE buf[FILE_COPY_BUFFER_SIZE];
    Int32 sz;

    if(cIOSeek(from, offset, SK_SET) < 0)
        return false;
    while(size) {
        if(size > FILE_COPY_BUFFER_SIZE)
            sz = FILE_COPY_BUFFER_SIZE;
        else
            sz = (Int32)size;
        if(cIORead(from, buf, sz) != sz)
            return false;
        if(cIOWrite(to, buf, sz) < 0)
            return false;
        size -= sz;
    }
    return true;
}

static bool traversalZipFileTree(ZipInfo* zipInfo, ZipTraversalFunc func, void* args) {
    ZipItem* root;
    ZipItem* it;

    root = zipInfo->root;
    it = root;
    while(true) {
        if(!func((ZipHandle)zipInfo, (ZipFile)it, args))
            return false;

        //step down
        if(it->flags & Flags_ZipItem_Directory) {
            if(it->off_childs) {
                it = (ZipItem*)it->off_childs;
                continue;
            }
        }

forward:
        //step forward
        if(it->next) {
            it = it->next;
            continue;
        }

        //step back
        if(it->parent == NULL)
            break;
        it = it->parent;
        goto forward;
    }
    return true;
}

static bool writeLocalFile(ZipHandle zip, ZipFile zfile, void* args) {
    ZipItem* item = (ZipItem*)zfile;
    BYTE localFileHeader[30];
    BYTE* p;
    Int32* iargs = (Int32*)args;
    ZipInfo* zipInfo = (ZipInfo*)zip;

    if(item->nameLen == 1 && item->name[0] == '/')
        return true;

    p = localFileHeader;
    cWriteLE32(p, localFileHeaderSignatrue);
    p += 4;
    //version
    cWriteLE16(p, 10);
    p += 2;
    //gflags
    cWriteLE16(p, 0);
    p += 2;
    //compress method.
    //note: compress ability not implement.
    cWriteLE16(p, item->compMode);
    p += 2;
    //last mod file time
    cWriteLE16(p, 0);
    p += 2;
    //last mod file date
    cWriteLE16(p, 0);
    p += 2;
    //crc
    cWriteLE32(p, item->crc32);
    p += 4;
    //compress size
    cWriteLE32(p, item->compSize);
    p += 4;
    //uncompress size
    cWriteLE32(p, item->uncompSize);
    p += 4;
    //filename length
    cWriteLE16(p, item->nameLen);
    p += 2;
    //extra field length
    cWriteLE16(p, 0);

    item->offsetInZipFile = iargs[1];
    if(cIOWrite(iargs[0], localFileHeader, sizeof(localFileHeader)) < 0)
        return false;
    if(cIOWrite(iargs[0], item->name, item->nameLen) < 0)
        return false;
    iargs[1] += sizeof(localFileHeader) + item->nameLen;
    if((item->flags & Flags_ZipItem_Directory) == 0) {
        if(item->flags & Flags_ZipItem_tmpFile) {
            if(!copyDataInFile(zipInfo->tmpFileDes, iargs[0], item->off_childs, item->specSize))
                return false;
            if(item->specSize < item->compSize) {
                if(cIOWrite(iargs[0], zipInfo->compBuffer + 4, item->compSize - item->specSize) < 0)
                    return false;
            }
        }
        else {
            if(cIOWrite(iargs[0], zipInfo->compBuffer + item->off_childs + 4, item->compSize) < 0)
                return false;
        }
        iargs[1] += item->compSize;
    }
    return true;
}

static bool writeCentralDirecotry(ZipHandle zip, ZipFile zfile, void* args) {
    ZipItem* item = (ZipItem*)zfile;
    BYTE* p;
    Int32* iargs = (Int32*)args;
    BYTE fileHeader[46];

    if(item->nameLen == 1 && item->name[0] == '/')
        return true;

    p = fileHeader;
    cWriteLE32(p, centralFileHeaderSignature);
    p += 4;
    //version made by
    cWriteLE16(p, 10);
    p += 2;
    //version need
    cWriteLE16(p, 10);
    p += 2;
    //gflags
    cWriteLE16(p, 0);
    p += 2;
    //compress mode
    cWriteLE16(p, item->compMode);
    p += 2;
    //last mod file time
    cWriteLE16(p, 0);
    p += 2;
    //last mod file date
    cWriteLE16(p, 0);
    p += 2;
    //crc
    cWriteLE32(p, item->crc32);
    p += 4;
    //compress size
    cWriteLE32(p, item->compSize);
    p += 4;
    //uncompress size
    cWriteLE32(p, item->uncompSize);
    p += 4;
    //filename length
    cWriteLE16(p, item->nameLen);
    p += 2;
    //extra field length
    cWriteLE16(p, 0);
    p += 2;
    //file comment length
    cWriteLE16(p, 0);
    p += 2;
    //disk number start
    cWriteLE16(p, 0);
    p += 2;
    //internal file attribute
    cWriteLE16(p, 0);
    p += 2;
    //external file attribute
    cWriteLE32(p, 0);
    p += 4;
    //relative offset
    cWriteLE32(p, item->offsetInZipFile);

    if(cIOWrite(iargs[0], fileHeader, sizeof(fileHeader)) < 0)
        return false;
    if(cIOWrite(iargs[0], item->name, item->nameLen) < 0)
        return false;
    iargs[2] += sizeof(fileHeader) + item->nameLen;
    return true;
}

static bool freeZipFileItem(void* key, UInt32 keySize, void* value, UInt32 size, void* args) {
    ZipItem* item;

    item = (ZipItem*)value;
    if(item->nameLen == 1/* && item->name[0] == '/'*/)
        return true;
//    printf("free file item:%d,%s\n", item->nameLen, item->name);
    cFree(item);
    return true;
}

bool cZipWrite(ZipHandle zip, const char* name) {
    ZipInfo* zipInfo;
    //0: zip file descriptor
    //1: zip file current seek position
    //2: central directory size
    Int32 args[3];
    BYTE centralDirEnd[22];
    BYTE* p;

    if(zip == 0 || name == NULL)
        return false;
    zipInfo = (ZipInfo*)zip;
    args[0] = cIOOpen(name, MADA_OF_CREATE | MADA_OF_WRITE | MADA_OF_TRUNC);
    if(args[0] < 0)
        return false;

    args[1] = 0;
    if(!traversalZipFileTree(zipInfo, writeLocalFile, args)) {
        cIOClose(args[0]);
        return false;
    }
    args[2] = 0;
    if(!traversalZipFileTree(zipInfo, writeCentralDirecotry, args)) {
        cIOClose(args[0]);
        return false;
    }

    p = centralDirEnd;
    cWriteLE32(p, centralDirEndSignature);
    p += 4;
    //number of this disk
    cWriteLE16(p, 0);
    p += 2;
    //number of the disk with the start of the central directory
    cWriteLE16(p, 0);
    p += 2;
    //total number of entries on this disk
    cWriteLE16(p, cHashCount(zipInfo->allItems)-1);
    p += 2;
    //total number of entries
    cWriteLE16(p, cHashCount(zipInfo->allItems)-1);
    p += 2;
    //size of the central directory
    cWriteLE32(p, args[2]);
    p += 4;
    //offset of central directory start
    cWriteLE32(p, args[1]);
    p += 4;
    //zipfile comment length
    cWriteLE16(p, 0);

    if(!cIOWrite(args[0], centralDirEnd, sizeof(centralDirEnd)) < 0) {
        cIOClose(args[0]);
        return false;
    }
    cIOClose(args[0]);
    return true;
}

bool cZipWriteByfd(ZipHandle zip, Int32 fd) {
    ZipInfo* zipInfo;
    //0: zip file descriptor
    //1: zip file current seek position
    //2: central directory size
    Int32 args[3];
    BYTE centralDirEnd[22];
    BYTE* p;

    if(zip == 0 || fd < 0)
        return false;
    zipInfo = (ZipInfo*)zip;
    args[0] = fd;//cIOOpen(name, MADA_OF_CREATE | MADA_OF_WRITE | MADA_OF_TRUNC);

    if(args[0] < 0)
        return false;

    args[1] = 0;
    if(!traversalZipFileTree(zipInfo, writeLocalFile, args)) {
        LOGE("1 cZipWriteByfd traversalZipFileTree error");
        //cIOClose(args[0]);
        return false;
    }
    args[2] = 0;
    if(!traversalZipFileTree(zipInfo, writeCentralDirecotry, args)) {
        LOGE("2 cZipWriteByfd traversalZipFileTree error");
        //cIOClose(args[0]);
        return false;
    }

    p = centralDirEnd;
    cWriteLE32(p, centralDirEndSignature);
    p += 4;
    //number of this disk
    cWriteLE16(p, 0);
    p += 2;
    //number of the disk with the start of the central directory
    cWriteLE16(p, 0);
    p += 2;
    //total number of entries on this disk
    cWriteLE16(p, cHashCount(zipInfo->allItems)-1);
    p += 2;
    //total number of entries
    cWriteLE16(p, cHashCount(zipInfo->allItems)-1);
    p += 2;
    //size of the central directory
    cWriteLE32(p, args[2]);
    p += 4;
    //offset of central directory start
    cWriteLE32(p, args[1]);
    p += 4;
    //zipfile comment length
    cWriteLE16(p, 0);

    if(!cIOWrite(args[0], centralDirEnd, sizeof(centralDirEnd)) < 0) {
        LOGE("cZipWriteByfd cIOWrite error errno=%d",errno);
        //cIOClose(args[0]);
        return false;
    }

    //cIOClose(args[0]);
    return true;
}


void cZipDestroy(ZipHandle zip) {
    ZipInfo* zipInfo;
    char tmpName[64];

    if(zip == 0)
        return;
    zipInfo = (ZipInfo*)zip;
    cHashTraversal(zipInfo->allItems, freeZipFileItem, NULL);
    cHashFree(zipInfo->allItems);
    if(zipInfo->zipfd >= 0)
        cIOClose(zipInfo->zipfd);
    if(zipInfo->tmpFileDes >= 0)
        cIOClose(zipInfo->tmpFileDes);
    getZipTmpFileName(zipInfo, tmpName, sizeof(tmpName));
    cIORemove(tmpName);
    cIORemoveDir(ZIP_TMP_PATH);
    cFree(zipInfo);
}


void cZipDestroyByMemory(ZipHandle zip) {
    ZipInfo* zipInfo;
    char tmpName[64];

    if(zip == 0)
        return;
    zipInfo = (ZipInfo*)zip;
    cHashTraversal(zipInfo->allItems, freeZipFileItem, NULL);
    cHashFree(zipInfo->allItems);
    if(zipInfo->zipfd >= 0)
        cIOClose(zipInfo->zipfd);
    if(zipInfo->tmpFileDes >= 0)
        cIOClose(zipInfo->tmpFileDes);
    getZipTmpFileName(zipInfo, tmpName, sizeof(tmpName));
    cIORemove(tmpName);
    cIORemoveDir(ZIP_TMP_PATH);
    //Use_static_memory
    //cFree(zipInfo);
}

static bool getCentralDirEnd(Int32 fd, UInt32* offset, BYTE* cenDirEnd) {
    UInt32 fsize;
    UInt32 bufSize = ZIP_CENTRAL_DIR_END_SIZE * 2;
    BYTE* buf;
    UInt32 seekPos;
    BYTE* sig;
    UInt32 nsig;

    fsize = cIOSize(fd);
    if(fsize < bufSize) {
        //correctly zip file can't small than 44 bytes.  ??
        return false;
    }
    buf = (BYTE*)cMalloc(bufSize);
    if(buf == NULL)
        return false;
    seekPos = fsize - bufSize;
    sig = buf + ZIP_CENTRAL_DIR_END_SIZE;
    while(true) {
        cIOSeek(fd, seekPos, SK_SET);
        cIORead(fd, buf, bufSize);
        while(sig >= buf) {
            nsig = cReadLE32(sig);
            if(nsig == centralDirEndSignature)
                break;
            --sig;
        }
        if(sig >= buf)
            break;
        if(seekPos == 0) {
            cFree(buf);
            return false;
        }
        if(seekPos < bufSize - 3) {
            sig = buf + seekPos - 1;
            seekPos = 0;
        }
        else {
            seekPos += 3;
            seekPos -= bufSize;
            sig = buf + bufSize - 4;
        }
    }

    cFree(buf);
    *offset = seekPos + (sig - buf);
    cIOSeek(fd, *offset, SK_SET);
    cIORead(fd, cenDirEnd, ZIP_CENTRAL_DIR_END_SIZE);
    return true;
}

static ZipInfo* createZipInfoFromData(BYTE* data, UInt32 size, UInt32 bufSize) {
    ZipInfo* zipInfo;
    BYTE* p;
    UInt32 sig;
    UInt16 compMode;
    UInt16 nameLen;
    UInt16 extraLen;
    UInt16 fileCommentLen;
    UInt32 compSize;
    UInt32 uncompSize;
    UInt32 zipFileOffset;
    char* name;
    ZipItem* item;

    zipInfo = (ZipInfo*)cZipCreate(bufSize);
    if(zipInfo == NULL)
        return NULL;
    p = data;
    while((UInt32)(p - data) < size) {
        sig = cReadLE32(p);
        if(sig != centralFileHeaderSignature) {
            cZipDestroy((ZipHandle)zipInfo);
            return NULL;
        }
        //signature: 4 bytes
        //version made by: 2 bytes
        //version extract: 2 bytes
        //general purpose flags: 2 bytes
        p += 10;
        compMode = cReadLE16(p);
        //compress method: 2 bytes
        //modify time: 2 bytes
        //modify date: 2 bytes
        //crc32: 4 bytes
        p += 10;
        compSize = cReadLE32(p);
        p += 4;
        uncompSize = cReadLE32(p);
        p += 4;
        nameLen = cReadLE16(p);
        p += 2;
        extraLen = cReadLE16(p);
        p += 2;
        fileCommentLen = cReadLE16(p);
        //file comment length: 2 bytes
        //disk number start: 2 bytes
        //internal file attr: 2 bytes
        //external file attr: 4 bytes
        p += 10;
        zipFileOffset = cReadLE32(p);
        p += 4;
        name = (char*)p;
        p += nameLen + extraLen + fileCommentLen;

        item = addNewZipItem(zipInfo, name, nameLen, compMode);
        if(item == NULL) {
            cZipDestroy((ZipHandle)zipInfo);
            return NULL;
        }
        if(name[nameLen - 1] == '/')
            item->flags |= Flags_ZipItem_Directory;
        else
            ++zipInfo->numFiles;
        item->compSize = compSize;
        item->uncompSize = uncompSize;
        item->offsetInZipFile = zipFileOffset;
    }
    return zipInfo;
}

ZipHandle cZipOpen(const char* fname, UInt32 bufSize) {
    ZipInfo* zipInfo;
    Int32 fd;
    UInt32 cenDirSize;
    UInt32 cenDirOffset;
    BYTE* cenDir;
    BYTE* p;
    UInt32 cenDirEndOffset;
    BYTE cenDirEnd[ZIP_CENTRAL_DIR_END_SIZE];

    fd = cIOOpen(fname, MADA_OF_READ);
    if(fd < 0)
        return 0;
    if(!getCentralDirEnd(fd, &cenDirEndOffset, cenDirEnd)) {
        cIOClose(fd);
        return 0;
    }

    //get central directory records
    p = cenDirEnd + 12;
    cenDirSize = cReadLE32(p);
    p += 4;
    cenDirOffset = cReadLE32(p);
    cenDir = (BYTE*)cMalloc(cenDirSize);
    if(cenDir == NULL) {
        cIOClose(fd);
        return 0;
    }
    cIOSeek(fd, cenDirOffset, SK_SET);
    cIORead(fd, cenDir, cenDirSize);
    zipInfo = createZipInfoFromData(cenDir, cenDirSize, bufSize);
    cFree(cenDir);
    if(zipInfo == NULL) {
        cIOClose(fd);
        return 0;
    }
    zipInfo->zipfd = fd;
    //TODO: initialize compress buffer
    return (ZipHandle)zipInfo;
}

ZipFile cZipGetFile(ZipHandle zip, const char* name, UInt16 nameLen) {
    ZipInfo* zipInfo;
    ZipItem* item;

    if(zip == 0 || name == NULL || nameLen == 0)
        return 0;
    zipInfo = (ZipInfo*)zip;
    item = (ZipItem*)cHashGet(zipInfo->allItems, name, nameLen, NULL);
    return (ZipFile)item;
}

UInt32 cZipGetFileSize(ZipHandle zip, ZipFile file) {
    ZipItem* item;

    if(zip == 0 || file == 0)
        return 0;
    item = (ZipItem*)file;
    return item->uncompSize;
}

#include <zlib.h>
UInt32 cZipGetFileData(ZipHandle zip, ZipFile file, BYTE* buf, UInt32 bufSize, UInt32* totalOutSize) {
    ZipInfo* zipInfo;
    ZipItem* item;
    BYTE* compData;
    z_stream strm;
    UInt32 ret;
    BYTE localFileHeader[ZIP_LOCAL_FILE_HEADER_SIZE];
    UInt16 fnameLen;
    UInt16 extraLen;

    /** simple implementation !!! */
    if(zip == 0 || file == 0 || buf == NULL || bufSize == 0)
        return 0;
    zipInfo = (ZipInfo*)zip;
    item = (ZipItem*)file;
    compData = (BYTE*)cMalloc(item->compSize);
    if(zipInfo->zipfd < 0)
        return 0;
    if(compData == NULL)
        return 0;
    if(cIOSeek(zipInfo->zipfd, item->offsetInZipFile, SK_SET) < 0) {
        cFree(compData);
        return 0;
    }
    if(cIORead(zipInfo->zipfd, localFileHeader, ZIP_LOCAL_FILE_HEADER_SIZE) < 0) {
        cFree(compData);
        return 0;
    }
    fnameLen = cReadLE16(localFileHeader + 26);
    extraLen = cReadLE16(localFileHeader + 28);
    if(cIOSeek(zipInfo->zipfd, fnameLen + extraLen, SK_CUR) < 0) {
        cFree(compData);
        return 0;
    }
    if(cIORead(zipInfo->zipfd, compData, item->compSize) < 0) {
        cFree(compData);
        return 0;
    }

    //temporary implementation!!!
    if(item->compMode == 0) {
        UInt32 outSize;

        outSize = item->compSize > bufSize ? bufSize : item->compSize;
        cMemcpy(buf, compData, outSize);
        ret = outSize;
    }
    else {
        cMemset(&strm, 0, sizeof(strm));
        strm.next_in = compData;
        strm.avail_in = item->compSize;
        strm.next_out = buf;
        strm.avail_out = bufSize;
        inflateInit2(&strm, -MAX_WBITS);
        inflate(&strm, Z_FINISH);
        ret = strm.total_out;
        inflateEnd(&strm);
    }
    cFree(compData);
    if(totalOutSize)
        *totalOutSize = item->uncompSize;
    return ret;
}

bool cZipIsDirectory(ZipHandle zip, ZipFile zfile) {
    ZipItem* item;

    if(zfile == 0)
        return false;
    item = (ZipItem*)zfile;
    if(item->flags & Flags_ZipItem_Directory)
        return true;
    return false;
}

const char* cZipGetFileName(ZipHandle zip, ZipFile zfile, UInt16* nameLen) {
    ZipItem* item;

    if(zfile == 0)
        return NULL;
    item = (ZipItem*)zfile;
    if(nameLen)
        *nameLen = item->nameLen;
    return item->name;
}

bool cZipTraversal(ZipHandle zip, ZipTraversalFunc func, void* args) {
    if(zip == 0 || func == NULL)
        return false;
    return traversalZipFileTree((ZipInfo*)zip, func, args);
}

UInt32 cZipFileNumber(ZipHandle zip) {
    ZipInfo* zipInfo;

    if(zip == 0)
        return 0;
    zipInfo = (ZipInfo*)zip;
    return zipInfo->numFiles;
}

UInt32 cZipDirNumber(ZipHandle zip) {
    ZipInfo* zipInfo;

    if(zip == 0)
        return 0;
    zipInfo = (ZipInfo*)zip;
    return cHashCount(zipInfo->allItems) - zipInfo->numFiles;
}

ZipFile cZipGetRoot(ZipHandle zip) {
    ZipInfo* zipInfo;

    if(zip == 0)
        return 0;
    zipInfo = (ZipInfo*)zip;
    return (ZipFile)zipInfo->root;
}

ZipFile cZipGetChild(ZipHandle zip, ZipFile zfile) {
    ZipItem* item;

    if(zfile == 0)
        return 0;
    item = (ZipItem*)zfile;
    if(item->flags & Flags_ZipItem_Directory)
        return item->off_childs;
    return 0;
}

ZipFile cZipGetNext(ZipHandle zip, ZipFile zfile) {
    ZipItem* item;

    if(zfile == 0)
        return 0;
    item = (ZipItem*)zfile;
    return (ZipFile)item->next;
}

ZipFile cZipGetParent(ZipHandle zip, ZipFile zfile) {
    ZipItem* item;

    if(zfile == 0)
        return 0;
    item = (ZipItem*)zfile;
    return (ZipFile)item->parent;
}
