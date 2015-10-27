#include <stdio.h>
#include <malloc.h>
#include <stdlib.h>
#include "cutils/log.h"
#include "libdex/DexFile.h"
#include "libdex/DexClass.h"
#include "libdex/CmdUtils.h"
#include "liropcode.h"
#include "str/cstr.h"
#include "d2d_l2d.h"
#include "d2d_d2l.h"
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include "cominc.h"
#include "comopt.h"
#include "xassert.h"
#include "io/cio.h"
#include "d2d_comm.h"
#include "dx_mgr.h"
#include "aoc_dx_mgr.h"
#include "prdf.h"
#include "dex.h"
#include "gra.h"
#include "dex_util.h"
#include "dex2ir.h"
#include "ir2dex.h"
#include "d2d_l2d.h"
#include "dex_driver.h"
#include "lir.h"

UInt32 gdb_compute_dataSize(D2Dpool* pool)
{
    UInt32 currentSize = cbsGetSize(pool->lbs);
    return (currentSize - pool->constSize)%4;
}

void copyLbs2Lbs(CBSHandle dst, CBSHandle src)
{
    void* data = cbsGetData(src);
    UInt32 size = cbsGetSize(src);

    cbsWrite(dst, data, size);
    return;
}

void fixDexClassDef(DexFile* pDexFile,
                    D2Dpool* pool,
                    const DexClassDef* pDexClassDef,
                    DexClassDef* newDexCd)
{
    newDexCd->classIdx = pDexClassDef->classIdx;
    newDexCd->accessFlags = pDexClassDef->accessFlags;
    newDexCd->superclassIdx = pDexClassDef->superclassIdx;

    if (pDexClassDef->interfacesOff == 0) {
        newDexCd->interfacesOff = 0;
    } else {
        newDexCd->interfacesOff = pool->typeListOff +
                        (pDexClassDef->interfacesOff - pool->dexTypeListOff);
    }

    newDexCd->sourceFileIdx = pDexClassDef->sourceFileIdx;

    if (pDexClassDef->annotationsOff == 0) {
        newDexCd->annotationsOff = 0;
    } else {
        newDexCd->annotationsOff = pool->annotationsDirectoryOff +
            (pDexClassDef->annotationsOff - pool->dexAnnotationsDirectoryOff);
    }

    if (pDexClassDef->staticValuesOff == 0) {
        newDexCd->staticValuesOff = 0;
    } else {
        newDexCd->staticValuesOff = pool->encodedArrayOff +
            (pDexClassDef->staticValuesOff - pool->dexEncodedArrayOff);
    }
}

static void copyFields(D2Dpool* pool, const DexClassData* pClassData)
{
    UInt32 i;
    UInt32 fieldCount = pClassData->header.staticFieldsSize;
    DexField* field;
    UInt32 fieldIdx = 0;

    for (i = 0; i < fieldCount; i++) {
        field = pClassData->staticFields + i;

        if (0 == i) {
            fieldIdx = field->fieldIdx;
            writeUnSignedLeb128ToCbs(pool->classDataCbs, field->fieldIdx);
        } else {
            writeUnSignedLeb128ToCbs(pool->classDataCbs,
                                     field->fieldIdx - fieldIdx);
            fieldIdx = field->fieldIdx;
        }

        writeUnSignedLeb128ToCbs(pool->classDataCbs, field->accessFlags);
    }

    fieldCount = pClassData->header.instanceFieldsSize;

    for (i = 0; i < fieldCount; i++) {
        field = pClassData->instanceFields + i;

        if (i == 0) {
            fieldIdx = field->fieldIdx;
            writeUnSignedLeb128ToCbs(pool->classDataCbs, field->fieldIdx);
        } else {
            writeUnSignedLeb128ToCbs(pool->classDataCbs,
                                     field->fieldIdx - fieldIdx);
            fieldIdx = field->fieldIdx;
        }

        writeUnSignedLeb128ToCbs(pool->classDataCbs, field->accessFlags);
    }
}

static void copyAndTransformMethod(
         D2Dpool* pool,
         DexFile* pDexFile,
         const DexMethod* pDexMethod,
         const DexClassData* pClassData,
         const DexClassDef* pClassDef,
         RegionMgr* rumgr)
{
    UInt32 methodIdx;
    List<DexRegion const*> rulist;
    for (UInt32 i = 0; i < pClassData->header.directMethodsSize; i++) {
        pDexMethod = pClassData->directMethods + i;

        if (pDexMethod->codeOff != 0) {
            d2rMethod(pool, pDexFile, pDexMethod, pClassDef, rumgr,
                      g_record_region_for_classs ? &rulist : NULL);
        } else {
            pool->codeOff = 0;
        }

        ASSERT0(pDexMethod->methodIdx < pDexFile->pHeader->methodIdsSize);

        if (0 == i) {
            methodIdx = pDexMethod->methodIdx;
            writeUnSignedLeb128ToCbs(pool->classDataCbs, pDexMethod->methodIdx);
        } else {
            writeUnSignedLeb128ToCbs(
                    pool->classDataCbs,
                    pDexMethod->methodIdx - methodIdx);
            methodIdx = pDexMethod->methodIdx;
        }

        writeUnSignedLeb128ToCbs(pool->classDataCbs, pDexMethod->accessFlags);
        writeUnSignedLeb128ToCbs(pool->classDataCbs, pool->codeOff);
    }

    for (UInt32 i = 0; i < pClassData->header.virtualMethodsSize; i++) {
        pDexMethod = pClassData->virtualMethods + i;

        if (pDexMethod->codeOff != 0) {
            d2rMethod(pool, pDexFile, pDexMethod, pClassDef, rumgr,
                      g_record_region_for_classs ? &rulist : NULL);
        } else {
            pool->codeOff = 0;
        }

        if (0 == i) {
            methodIdx = pDexMethod->methodIdx;
            writeUnSignedLeb128ToCbs(pool->classDataCbs, pDexMethod->methodIdx);
        } else {
            writeUnSignedLeb128ToCbs(
                    pool->classDataCbs,
                    pDexMethod->methodIdx - methodIdx);
            methodIdx = pDexMethod->methodIdx;
        }

        writeUnSignedLeb128ToCbs(pool->classDataCbs, pDexMethod->accessFlags);
        writeUnSignedLeb128ToCbs(pool->classDataCbs, pool->codeOff);
    }
}

static void convertClassData(
        DexFile* pDexFile,
        D2Dpool* pool,
        const DexClassDef* pDexClassDef,
        RegionMgr* rumgr)
{
    const BYTE* pEncodedData = NULL;
    const DexClassData* pClassData = NULL;
    const DexMethod* pDexMethod = NULL;

    pEncodedData = dexGetClassData(pDexFile, pDexClassDef);
    pClassData = dexReadAndVerifyClassData(&pEncodedData, NULL);

    //write to the static fields size
    writeUnSignedLeb128ToCbs(
        pool->classDataCbs,
        pClassData->header.staticFieldsSize);

    writeUnSignedLeb128ToCbs(
        pool->classDataCbs,
        pClassData->header.instanceFieldsSize);

    writeUnSignedLeb128ToCbs(
        pool->classDataCbs,
        pClassData->header.directMethodsSize);

    writeUnSignedLeb128ToCbs(
        pool->classDataCbs,
        pClassData->header.virtualMethodsSize);

    copyFields(pool, pClassData);

    copyAndTransformMethod(
        pool,
        pDexFile,
        pDexMethod,
        pClassData,
        pDexClassDef,
        rumgr);
}

static D2Dpool* poolInfoInit()
{
    D2Dpool* pool = (D2Dpool*)malloc(sizeof(D2Dpool));
    memset(pool, 0, sizeof(D2Dpool));

    pool->classDataCbs = cbsInitialize(0);
    pool->lbs = cbsInitialize(0);
    pool->mapLbs = cbsInitialize(0);
    pool->otherLbs = cbsInitialize(0);

    return pool;
}

static void freePool(D2Dpool* pool, bool freeBuffer)
{
    if(freeBuffer && pool->lbs)
        cbsDestroy(pool->lbs);
    if(pool->classDataCbs)
        cbsDestroy(pool->classDataCbs);
    if(pool->mapLbs)
        cbsDestroy(pool->mapLbs);
    if(pool->otherLbs)
        cbsDestroy(pool->otherLbs);

    free(pool);

    return;
}

static Int32 writeToFile(D2Dpool* pool, int outFd, long* fileLen, bool ifOpt)
{
    Int32 ret = -1;
    BYTE* data;
    UInt32 dataSize;
    if ((ret = cIOSeek(outFd, 0, SK_SET)) == -1) {
        LOGE("error: writeTofile: fail to seek fd to the begin of the file.\n");
        goto END;
    }

    //Write the pass header to the file.
    if (ifOpt) {
        UInt32 optSize = sizeof(DexOptHeader);
        char buff[optSize];
        memset(buff, 0xff, optSize);
        cIOWrite(outFd, buff, optSize);
    }

    if ((ret = cIOTruncate(outFd, 0)) == -1) {
        LOGE("error: writeTofile: fail to truncate fd to the begin of the file.\n");
        goto END;
    }

    data = cbsGetData(pool->lbs);
    dataSize = cbsGetSize(pool->lbs);

    if ((UInt32)cIOWrite(outFd, data, dataSize) != dataSize) {
        LOGE("error: writeTofile: write file error!\n ");
        goto END;
    }

    ASSERT0(dataSize == pool->currentSize);
    ret = 0;
    *fileLen = dataSize;

END:
    return ret;
}

static void createHeader(DexFile* pDexFile, D2Dpool* pool)
{
    const DexHeader* header = pDexFile->pHeader;
    BYTE signatur[20];
    memset(signatur, 0, sizeof(BYTE)*20);
    int ENDIAN_CONSTANT = 0x12345678;

    ASSERT0(pool->currentSize == 0);
    /*the buff size*/
    pool->currentSize += 0x70;

    UInt64 magic = *((UInt64*)(header->magic));
    cbsWrite64(pool->lbs, magic); /*for dex magic*/
    cbsWrite32(pool->lbs, 0); /*for checksum*/
    cbsWrite(pool->lbs, signatur, 20);/*for signature*/

    /*TODO the fileSize should be recomputed*/
    cbsWrite32(pool->lbs, 0);
    cbsWrite32(pool->lbs, 0x70); /*header size*/
    cbsWrite32(pool->lbs, ENDIAN_CONSTANT); /*endian_tag*/

    /*TODO the linksize is 0*/
    cbsWrite32(pool->lbs, 0); /*link size*/
    cbsWrite32(pool->lbs, 0); /*link_off*/

    /*TODO the map off is 0*/
    cbsWrite32(pool->lbs, 0);
    /*string ids size*/
    cbsWrite32(pool->lbs, header->stringIdsSize);
    cbsWrite32(pool->lbs, header->stringIdsOff);
    pool->stringIdsOff = header->stringIdsOff;
    pool->currentSize += (header->stringIdsSize) * (sizeof(DexStringId));

    /*type ids size */
    cbsWrite32(pool->lbs, header->typeIdsSize);
    cbsWrite32(pool->lbs, header->typeIdsOff);
    pool->typeIdsOff = header->typeIdsOff;
    pool->currentSize += (header->typeIdsSize) * (sizeof(DexTypeId));

    /*proto ids size*/
    cbsWrite32(pool->lbs, header->protoIdsSize);
    cbsWrite32(pool->lbs, header->protoIdsOff);
    pool->protoIdOff = header->protoIdsOff;
    pool->currentSize += (header->protoIdsSize) * (sizeof(DexProtoId));

    /*field ids size*/
    cbsWrite32(pool->lbs, header->fieldIdsSize);
    cbsWrite32(pool->lbs, header->fieldIdsOff);
    pool->fieldIdOff = header->fieldIdsOff;
    pool->currentSize += (header->fieldIdsSize) * (sizeof(DexFieldId));

    /*method ids size*/
    cbsWrite32(pool->lbs, header->methodIdsSize);
    cbsWrite32(pool->lbs, header->methodIdsOff);
    pool->methodIdOff = header->methodIdsOff;
    pool->currentSize += (header->methodIdsSize) * (sizeof(DexMethodId));

    /*class ids size*/
    cbsWrite32(pool->lbs, header->classDefsSize);
    cbsWrite32(pool->lbs, header->classDefsOff);
    pool->classEntryOff = pool->currentSize;
    ASSERT0(header->classDefsOff == pool->classEntryOff);
    pool->currentSize += (header->classDefsSize) * (sizeof(DexClassDef));

    cbsWrite32(pool->lbs, header->dataSize);
    cbsWrite32(pool->lbs, header->dataOff);

    /*to make sure the header's size is right*/
    ASSERT0(0x70 == cbsGetSize(pool->lbs));

    return;
}

static void copyIdItem(DexFile* pDexFile, D2Dpool* pool)
{
    UInt32 copySize;
    void* data;

    /*string id*/
    copySize = pDexFile->pHeader->stringIdsSize * sizeof(DexStringId);
    // If id size is zero, do not copy and asset.
    if (copySize > 0) {
        data = (void*)(pDexFile->baseAddr + pDexFile->pHeader->stringIdsOff);
        ASSERT0(pool->stringIdsOff == (cbsGetSize(pool->lbs)));
        cbsWrite(pool->lbs, data, copySize);
    }

    //typeId
    copySize = pDexFile->pHeader->typeIdsSize * sizeof(DexTypeId);
    // If id size is zero, do not copy and asset.
    if (copySize > 0) {
        data = (void*)(pDexFile->baseAddr + pDexFile->pHeader->typeIdsOff);
        ASSERT0(pool->typeIdsOff== (cbsGetSize(pool->lbs)));
        cbsWrite(pool->lbs, data, copySize);
    }

    //protoId
    copySize = pDexFile->pHeader->protoIdsSize * sizeof(DexProtoId);
    // If id size is zero, do not copy and asset.
    if (copySize > 0) {
        data = (void*)(pDexFile->baseAddr + pDexFile->pHeader->protoIdsOff);
        ASSERT0(pool->protoIdOff == (cbsGetSize(pool->lbs)));
        cbsWrite(pool->lbs, data, copySize);
    }

    //fieldId
    copySize = pDexFile->pHeader->fieldIdsSize * sizeof(DexFieldId);
    // If id size is zero, do not copy and asset.
    if (copySize > 0) {
        data = (void*)(pDexFile->baseAddr + pDexFile->pHeader->fieldIdsOff);
        ASSERT0(pool->fieldIdOff == (cbsGetSize(pool->lbs)));
        cbsWrite(pool->lbs, data, copySize);
    }

    //methodId
    copySize = pDexFile->pHeader->methodIdsSize * sizeof(DexMethodId);
    // If id size is zero, do not copy and asset.
    if (copySize > 0) {
        data = (void*)(pDexFile->baseAddr + pDexFile->pHeader->methodIdsOff);
        ASSERT0(pool->methodIdOff == (cbsGetSize(pool->lbs)));
        cbsWrite(pool->lbs, data, copySize);
    }

    //TODO we do not record the class entry now, but leave space for it.
    cbsSeek(pool->lbs, pool->currentSize);

    pool->dataOff = pool->currentSize;
    pool->constSize = pool->currentSize;
}

static void fixAndCopyStringData(
        DexFile* pDexFile,
        D2Dpool* pool,
        UInt32* insideOffset)
{
    UInt32 i;
    UInt32 count = pool->stringDataSize;
    DexStringId* item;
    const char* str;
    ASSERT0(count == pDexFile->pHeader->stringIdsSize);

    //cbsSeek(pool->otherLbs, pool->stringDataOff);

    UInt32 len;
    UInt32 lebLen;
    BYTE lebBuf[5];

    for (i = 0; i < count; i++) {
        item = (DexStringId*)(pDexFile->pStringIds + i);
        str = dexStringAndSizeById(pDexFile, i, &len);

        *insideOffset += writeUnSignedLeb128ToCbs(pool->otherLbs, len);

        lebLen = cStrlen(str);
        cbsWrite(pool->otherLbs, str, lebLen);
        cbsWrite8(pool->otherLbs, 0);

        *insideOffset += lebLen + 1;
    }
}

static void copyDexMiscData(DexFile* pDexFile, D2Dpool* pool)
{
    const DexMapList* pDexMap;
    const DexMapItem* item;
    UInt32 count;
    bool shouldCopy = false;
    bool isStringData = false;
    bool writeNow= false;

    UInt32 startOffset = 0;
    UInt32 endOffset = 0;

    pDexMap = dexGetMap(pDexFile);
    item = pDexMap->list;
    count = pDexMap->size;

    CBSHandle mapLbs = pool->mapLbs;
    cbsWrite32(mapLbs, pDexMap->size);
    UInt32 offset = 0;
    UInt32 insideOffset = 0;

    while (count--) {
        switch(item->type) {
        case kDexTypeHeaderItem:
        case kDexTypeStringIdItem:
        case kDexTypeTypeIdItem:
        case kDexTypeProtoIdItem:
        case kDexTypeFieldIdItem:
        case kDexTypeMethodIdItem:
        case kDexTypeClassDefItem:
        case kDexTypeMapList:
        case kDexTypeClassDataItem:
        case kDexTypeCodeItem:
            offset = item->offset;
            break;
        case kDexTypeDebugInfoItem:
            pool->dexDebugItemOff = item->offset;
            offset = pool->debugItemOff = insideOffset;
            shouldCopy = true;
            break;
        case kDexTypeTypeList:
            pool->dexTypeListOff = item->offset;
            offset = pool->typeListOff = insideOffset;
            shouldCopy = true;
            break;
        case kDexTypeAnnotationSetRefList:
            pool->dexAnnotationSetRefListOff = item->offset;
            pool->annotationSetRefListSize = item->size;
            offset = pool->annotationSetRefListOff = pool->currentSize;
            shouldCopy = true;
            writeNow = true;
            break;
        case kDexTypeAnnotationSetItem:
            pool->dexAnnotationSetOff = item->offset;
            pool->annotationSetSize = item->size;
            offset = pool->annotationSetOff = pool->currentSize;
            shouldCopy = true;
            writeNow = true;
            break;
        case kDexTypeStringDataItem:
            pool->dexStringDataOff = item->offset;
            pool->stringDataSize = item->size;
            offset = pool->stringDataOff = insideOffset;
            shouldCopy = true;
            break;
        case kDexTypeAnnotationItem:
            pool->dexAnnotationItemOff = item->offset;
            offset = pool->annotationItemOff = insideOffset;
            shouldCopy = true;
            break;
        case kDexTypeEncodedArrayItem:
            pool->dexEncodedArrayOff = item->offset;
            offset = pool->encodedArrayOff = insideOffset;
            shouldCopy = true;
            break;
        case kDexTypeAnnotationsDirectoryItem:
            pool->dexAnnotationsDirectoryOff = item->offset;
            pool->annotationsDirectorySize = item->size;
            offset = pool->annotationsDirectoryOff = insideOffset;
            shouldCopy = true;
            break;
        default:
            printf("unknown map item type %04x\n",item->type);
            ASSERT0(false);
            return ;
        }

        cbsWrite16(mapLbs, item->type);
        cbsWrite16(mapLbs, item->unused);
        cbsWrite32(mapLbs, item->size);
        cbsWrite32(mapLbs, offset);

        startOffset = item->offset;
        item++;

        if (shouldCopy) {
            //Bug: we should process it when we are the last item.
            endOffset = count == 0? pDexFile->pHeader->fileSize : item->offset;

            void* data = (void*)(pDexFile->baseAddr + startOffset);

            //Because the item is in line, so we can use
            //current_item->offset - pre_item->offset
            //to compute item's length.
            UInt32 copySize = endOffset - startOffset;

            if (writeNow) {
                cbsWrite(pool->lbs, data, copySize);
                pool->currentSize += copySize;
            } else {
                cbsWrite(pool->otherLbs, data, copySize);
                insideOffset += copySize;
            }

            shouldCopy = false;
            writeNow = false;
        }
    }

    //Copy the link data after the map data.
    if (pDexFile->pHeader->linkSize != 0) {
        pool->linkOff = cbsGetSize(mapLbs);
        const void* data = pDexFile->baseAddr + pDexFile->pHeader->linkOff;
        cbsWrite(mapLbs, data, pDexFile->pHeader->linkSize);
    }
}

static void fixStringIdItem(DexFile* pDexFile, D2Dpool* pool)
{
    UInt32 i, count;
    BYTE* data;
    BYTE* ptr;
    UInt32 newOffset;

    cbsSeek(pool->lbs, pool->stringIdsOff);
    count = pDexFile->pHeader->stringIdsSize;

    ptr = data = cbsGetData(pool->lbs);
    data += pool->stringDataOff;

    UInt32 utr16Size;

    const char* str;
    for (i = 0; i < count; i++) {
        newOffset = data - ptr;

        str = dexStringById(pDexFile, i);
        readUnsignedLeb128((const BYTE**)(&data));
        data += cStrlen(str) + 1;

        cbsWrite32(pool->lbs, newOffset);
    }
}

static void fixProtoIdItem(DexFile* pDexFile, D2Dpool* pool)
{
    UInt32 i;
    UInt32 count = pDexFile->pHeader->protoIdsSize;
    DexProtoId* item;
    UInt32 newOffSet;

    for (i = 0; i < count; i++) {
        item = (DexProtoId*)(pDexFile->pProtoIds + i);

        //0 if the prototype has no parameters.
        if (item->parametersOff != 0) {
            newOffSet = pool->typeListOff +
                        (item->parametersOff - pool->dexTypeListOff);
            cbsSeek(pool->lbs, pool->protoIdOff + i * sizeof(DexProtoId) + 8);
            cbsWrite32(pool->lbs, newOffSet);
        }
    }
}

static void fixAnnotationDirectoryItem(DexFile* pDexFile, D2Dpool* pool)
{
   UInt32 i, j, itemOff, newOffset, count;

   DexAnnotationsDirectoryItem* pAnnoDir;

   const DexFieldAnnotationsItem* pAllFieldAnnoItems;
   const DexMethodAnnotationsItem *pAllMethodAnnoItems;
   const DexParameterAnnotationsItem* pAllParamAnnoItems;

   count = pool->annotationsDirectorySize;

   itemOff = 0;
   for (i = 0; i < count; i++) {
       pAnnoDir = (DexAnnotationsDirectoryItem*)(pDexFile->baseAddr +
                                   pool->dexAnnotationsDirectoryOff + itemOff);
       if (pAnnoDir->classAnnotationsOff != 0) {
           newOffset = pool->annotationSetOff +
                   (pAnnoDir->classAnnotationsOff - pool->dexAnnotationSetOff);
           cbsSeek(pool->lbs, pool->annotationsDirectoryOff + itemOff);
           cbsWrite32(pool->lbs, newOffset);
       }

       itemOff += sizeof(DexAnnotationsDirectoryItem);

       pAllFieldAnnoItems = dexGetFieldAnnotations(pDexFile, pAnnoDir);

       //field annotation.
       for (j = 0; j < pAnnoDir->fieldsSize; j++) {
           if (pAllFieldAnnoItems[j].annotationsOff != 0) {
               newOffset = pool->annotationSetOff +
                       (pAllFieldAnnoItems[j].annotationsOff -
                        pool->dexAnnotationSetOff);
               cbsSeek(pool->lbs, pool->annotationsDirectoryOff + itemOff + 4);
               cbsWrite32(pool->lbs, newOffset);
           }
           itemOff += sizeof(DexFieldAnnotationsItem);
       }

       pAllMethodAnnoItems = dexGetMethodAnnotations(pDexFile, pAnnoDir);

       //method annotation.
       for (j = 0; j < pAnnoDir->methodsSize; j++) {
           if (pAllMethodAnnoItems[j].annotationsOff != 0) {
               newOffset = pool->annotationSetOff +
                           (pAllMethodAnnoItems[j].annotationsOff -
                            pool->dexAnnotationSetOff);
               cbsSeek(pool->lbs, pool->annotationsDirectoryOff + itemOff + 4);
               cbsWrite32(pool->lbs, newOffset);
           }

           itemOff += sizeof(DexMethodAnnotationsItem);
       }

       pAllParamAnnoItems = dexGetParameterAnnotations(pDexFile, pAnnoDir);
       for (j = 0; j < pAnnoDir->parametersSize; j++) {
           if (pAllParamAnnoItems[j].annotationsOff != 0) {
               newOffset = pool->annotationSetRefListOff +
                       (pAllParamAnnoItems[j].annotationsOff -
                        pool->dexAnnotationSetRefListOff);

               cbsSeek(pool->lbs, pool->annotationsDirectoryOff + itemOff + 4);
               cbsWrite32(pool->lbs, newOffset);
           }
           itemOff += sizeof(DexParameterAnnotationsItem);
       }
   }
}

static void fixAnnotationSetRefListItem(DexFile* pDexFile, D2Dpool* pool)
{
    UInt32 i, j;
    UInt32 count;
    UInt32 newOffset;
    UInt32 itemOff;

    DexAnnotationSetRefList* pAnnoRefList;

    itemOff = 0;
    count = pool->annotationSetRefListSize;

    for (i = 0; i < count; i++) {
        pAnnoRefList = (DexAnnotationSetRefList*)(pDexFile->baseAddr +
                                pool->dexAnnotationSetRefListOff + itemOff);

        itemOff += 4;
        for (j = 0; j < pAnnoRefList->size; j++) {
            if (pAnnoRefList->list[j].annotationsOff != 0) {
                newOffset = pool->annotationSetOff +
                            (pAnnoRefList->list[j].annotationsOff -
                             pool->dexAnnotationSetOff);
                cbsSeek(pool->lbs, pool->annotationSetRefListOff + itemOff);
                cbsWrite32(pool->lbs, newOffset);
            }
            itemOff += sizeof(DexAnnotationSetRefItem);
        }
    }
}

static void fixAnnotationSetItem(DexFile* pDexFile, D2Dpool* pool)
{
    UInt32 i, j;
    UInt32 count;
    UInt32 newOffset;
    UInt32 itemOff;
    DexAnnotationSetItem* pAnnoSet;

    itemOff = 0;
    count = pool->annotationSetSize;

    for (i = 0; i < count; i++) {
        pAnnoSet = (DexAnnotationSetItem*)(pDexFile->baseAddr +
                                           pool->dexAnnotationSetOff + itemOff);
        itemOff += 4;

        for (j = 0; j < pAnnoSet->size; j++) {
            if(pAnnoSet->entries[j] != 0) {
                newOffset = pool->annotationItemOff +
                                (pAnnoSet->entries[j] -
                                 pool->dexAnnotationItemOff);
                cbsSeek(pool->lbs, pool->annotationSetOff + itemOff);
                cbsWrite32(pool->lbs, newOffset);
            }

            itemOff += sizeof(UInt32);
        }
    }
}

static void fixDexSetionOffset(DexFile* pDexFile, D2Dpool* pool)
{
    fixStringIdItem(pDexFile, pool);
    fixProtoIdItem(pDexFile, pool);
    fixAnnotationDirectoryItem(pDexFile, pool);
    fixAnnotationSetRefListItem(pDexFile, pool);
    fixAnnotationSetItem(pDexFile, pool);
}

static void copyClassData(D2Dpool* pool)
{
    UInt32 copySize;
    void* data;

    ASSERT0(pool->currentSize == cbsGetSize(pool->lbs));

    pool->classDataOffset = pool->currentSize;
    data = cbsGetData(pool->classDataCbs);
    copySize = cbsGetSize(pool->classDataCbs);

    cbsWrite(pool->lbs, data, copySize);
    pool->currentSize += copySize;

    return;
}

static void processClass(DexFile* pDexFile, D2Dpool* pool)
{
    const DexClassDef* pDexClassDef;
    UInt32 clsNumber = pDexFile->pHeader->classDefsSize;

    ASSERT0(pool->currentSize == cbsGetSize(pool->lbs));
    DexClassDef nDexCd;
    memset(&nDexCd, 0, sizeof(DexClassDef));

    pool->codeItemOff = pool->currentSize;
    UInt32 size = 0;

    DexRegionMgr * rumgr = NULL;
    Region * topru = NULL;
    if (g_do_ipa) {
        g_do_call_graph = true;
        g_collect_debuginfo = true;
        rumgr = new DexRegionMgr();
        rumgr->initVarMgr();
        rumgr->init();
        topru = rumgr->newRegion(RU_PROGRAM);
        rumgr->set_region(topru);
          topru->set_ru_var(rumgr->get_var_mgr()->registerVar(
                           ".dex",
                           rumgr->get_type_mgr()->getMCType(0),
                           0,
                           VAR_GLOBAL|VAR_FAKE));
    }

    for (UInt32 i = 0; i < clsNumber; i++) {
        pDexClassDef = dexGetClassDef(pDexFile, i);

        if (pDexClassDef->classDataOff == 0) { continue; }

        nDexCd.classDataOff = cbsGetSize(pool->classDataCbs);
        cbsSeek(pool->lbs, pool->classEntryOff + i * sizeof(DexClassDef));
        cbsWrite(pool->lbs, &nDexCd, sizeof(DexClassDef));

        //printf("%s\n", dexGetClassDescriptor(pDexFile, pDexClassDef));

        convertClassData(pDexFile, pool, pDexClassDef, rumgr);
        size++;
    }

    if (g_do_ipa) {
        rumgr->processProgramRegion(topru);
        delete rumgr;
    }

    pool->updateClassDataSize = 0;
    if (size == clsNumber) {
        // if no offset == 0, use clsnumber as class data item's size.
        pool->updateClassDataSize = size;
    }
}

static void fixClassDefOffset(DexFile* pDexFile, D2Dpool* pool)
{
    UInt32 i;
    UInt32 count;
    DexClassDef* classDef;
    BYTE* baseData;

    baseData = cbsGetData(pool->lbs);
    count = pDexFile->pHeader->classDefsSize;

    for (i = 0; i < count; i++) {
        classDef = (DexClassDef*)(pool->classEntryOff +
                                  baseData + i * sizeof(DexClassDef));
        cbsSeek(pool->lbs, pool->classEntryOff + i * sizeof(DexClassDef) + 24);
        cbsWrite32(pool->lbs, (pool->classDataOffset + classDef->classDataOff));
    }
}

static void fixDexHeader(D2Dpool* pool)
{
    UInt32 fileSize = cbsGetSize(pool->lbs);
    ASSERT0(fileSize == pool->currentSize);

    cbsSeek(pool->lbs, 32);
    cbsWrite32(pool->lbs, fileSize);

    UInt32 dataSize = fileSize - pool->constSize;

    //data size
    cbsSeek(pool->lbs, (0x70 - 8));
    cbsWrite32(pool->lbs, dataSize);
    cbsWrite32(pool->lbs, pool->dataOff);

    //For map offset and link data offset.
    cbsSeek(pool->lbs, 48);
    cbsWrite32(pool->lbs, pool->linkOff);
    cbsWrite32(pool->lbs, pool->mapOff);

    //Note this must be done at end.
    void* data = cbsGetData(pool->lbs);
    DexHeader* pHeader = (DexHeader*)data;
    UInt32 crcSum = dexComputeChecksum(pHeader);

    //for crc
    cbsSeek(pool->lbs, 8);
    cbsWrite32(pool->lbs, crcSum);
}

static int compareMapItem(const void* a, const void* b)
{
   return (*(const DexMapItem**)a)->offset - (*(const DexMapItem**)b)->offset;
}

static void fixAndCopyMapItemOffset(DexFile* pDexFile, D2Dpool* pool)
{
   const DexMapList* dexMapList = dexGetMap(pDexFile);
   const DexMapItem* dexMapItem = dexMapList->list;
   UInt32 lastOffset = 0xffffffff;
   DexMapList* mapList;
   DexMapItem* item;
   DexMapItem* lastItem = NULL;

   //List the map items and change the item's offset.
   mapList = (DexMapList*)cbsGetData(pool->mapLbs);
   ASSERT0(mapList->size == dexMapList->size);

   item = mapList->list;
   Int32 count = mapList->size;

   DexMapItem* itemArray[count];
   Int32 idx = 0;

   while (count--) {
       ASSERT0(item->type == dexMapItem->type);

       switch (item->type) {
       //These items' offsets are same to the dex file.
       case kDexTypeHeaderItem:
       case kDexTypeStringIdItem:
       case kDexTypeTypeIdItem:
       case kDexTypeProtoIdItem:
       case kDexTypeFieldIdItem:
       case kDexTypeMethodIdItem:
           break;
       case kDexTypeClassDefItem:
           item->offset = pool->classEntryOff;
           break;
       case kDexTypeDebugInfoItem:
           item->offset = pool->debugItemOff;
           break;
       case kDexTypeTypeList:
           item->offset = pool->typeListOff;
           break;
       case kDexTypeAnnotationSetRefList:
           item->offset = pool->annotationSetRefListOff;
           break;
       case kDexTypeAnnotationSetItem:
           item->offset = pool->annotationSetOff;
           break;
       case kDexTypeStringDataItem:
           item->offset = pool->stringDataOff;
           break;
       case kDexTypeAnnotationItem:
           item->offset = pool->annotationItemOff;
           break;
       case kDexTypeEncodedArrayItem:
           item->offset = pool->encodedArrayOff;
           break;
       case kDexTypeAnnotationsDirectoryItem:
           item->offset = pool->annotationsDirectoryOff;
           break;
       case kDexTypeMapList:
           item->offset = pool->mapOff;
           break;
       case kDexTypeClassDataItem:
           item->offset = pool->classDataOffset;

           //Fix size to be identical to class data
           //size writen in WriteCodeItem.
           if (pool->updateClassDataSize) {
              item->size = pool->updateClassDataSize;
           }
           break;
       case kDexTypeCodeItem:
           item->offset = pool->codeItemOff;
           break;
       default:
           ASSERT0(false);
           return ;
       }
       itemArray[idx++] = item;
       item++;
       dexMapItem++;
   }

   //Bug: The Item in the maps shoule be ordered by the offset
   qsort(itemArray, idx, sizeof(DexMapItem*), compareMapItem);
   cbsSeek(pool->lbs, pool->currentSize);

   //write map item section to lbs
   //write mapList size
   cbsWrite32(pool->lbs, idx);

   //write the sorted items
   for (Int32 i = 0; i < idx; i++) {
       DexMapItem* item_ = itemArray[i];
       cbsWrite16(pool->lbs, item_->type);
       cbsWrite16(pool->lbs, item_->unused);
       cbsWrite32(pool->lbs, item_->size);
       cbsWrite32(pool->lbs, item_->offset);
   }
   pool->currentSize = cbsGetSize(pool->lbs);
}

static void fixOtherItemOffset(D2Dpool* pool, UInt32 otherOffset)
{
    pool->annotationsDirectoryOff = otherOffset + pool->annotationsDirectoryOff;
    pool->typeListOff = otherOffset + pool->typeListOff;
    pool->stringDataOff = otherOffset + pool->stringDataOff;
    pool->debugItemOff = otherOffset + pool->debugItemOff;
    pool->annotationItemOff = otherOffset + pool->annotationItemOff;
    pool->encodedArrayOff = otherOffset + pool->encodedArrayOff;
}

static void copyOtherData(D2Dpool* pool)
{
    ASSERT0(pool->currentSize == cbsGetSize(pool->lbs));

    cbsSeek(pool->lbs, pool->currentSize);
    UInt32 otherOffset = pool->currentSize;

    void * data = cbsGetData(pool->otherLbs);
    UInt32 copySize = cbsGetSize(pool->otherLbs);

    ASSERT0(pool->currentSize == cbsGetSize(pool->lbs));
    cbsWrite(pool->lbs, data, copySize);
    pool->currentSize += copySize;

    ASSERT0(pool->currentSize == cbsGetSize(pool->lbs));
    fixOtherItemOffset(pool, otherOffset);

    ASSERT0(pool->currentSize == cbsGetSize(pool->lbs));
    return;
}

static void setMapItemOffset(D2Dpool* pool)
{
    //BUG: Incur antutu install failed.
    alignLbs(pool->lbs);
    pool->currentSize = cbsGetSize(pool->lbs);
    pool->mapOff = pool->currentSize;

    /* Because we shoule sort the map items after all item's offset
    have been make sure. So do not copy the map item here but just
    set the map offset. */
    return;
}

static void fixCodeItem(
        DexFile* pDexFile,
        D2Dpool* pool,
        const DexClassDef* pDexClassDef)
{
    void* data = cbsGetData(pool->lbs);

    UInt32 count = pool->codeItemSize;
    DexCode* pCode;
    UInt32 prevSize = 0;
    UInt32 i;

    for (i = 0; i < count; i++) {
        pCode = (DexCode*)((BYTE*)data + pool->codeItemOff + prevSize);

        while (((ULong)pCode & 3) != 0) {
            ASSERT0((*(BYTE*)pCode) == 0);
            pCode = (DexCode*)((BYTE*)pCode + 1);
            prevSize++;
        }

        if (pCode->debugInfoOff) {
            pCode->debugInfoOff = pool->debugItemOff +
                                  (pCode->debugInfoOff -
                                   pool->dexDebugItemOff);
        }

        prevSize += dexGetDexCodeSize((const DexCode*)pCode);
    }

    return;
}

static void fixClassOffset(DexFile* pDexFile, D2Dpool* pool)
{
    const DexClassDef* pDexClassDef;
    const void* data;
    DexClassDef nDexCd;
    DexClassDef* dexCd;
    UInt32 i;
    UInt32 clsNumber = pDexFile->pHeader->classDefsSize;

    data = cbsGetData(pool->lbs);
    for (i = 0; i < clsNumber; i++) {
        pDexClassDef = dexGetClassDef(pDexFile, i);

        //to fix the class def entry.
        dexCd = (DexClassDef*)((BYTE*)data +
                               pool->classEntryOff +
                               i * sizeof(DexClassDef));

        fixDexClassDef(pDexFile, pool, pDexClassDef, &nDexCd);

        if (pDexClassDef->classDataOff == 0)
            nDexCd.classDataOff = 0;
        else
            nDexCd.classDataOff = dexCd->classDataOff + pool->classDataOffset;

        cbsSeek(pool->lbs, pool->classEntryOff + i * sizeof(DexClassDef));
        cbsWrite(pool->lbs, &nDexCd, sizeof(DexClassDef));
    }

    fixCodeItem(pDexFile, pool, pDexClassDef);
}


static D2Dpool* doCopyAndFixup(DexFile* pDexFile, char const* dexfilename)
{
    UInt32 i;
    UInt32 clsNumber = 0;
    D2Dpool* pool = poolInfoInit();

    //to write the items and fix the offset.
    createHeader(pDexFile, pool);

    //copy the setion id items.
    copyIdItem(pDexFile, pool);

    //copy the item from the map item.
    copyDexMiscData(pDexFile, pool);

    g_do_dex_ra = false;

    //transform class and write the code item.
    processClass(pDexFile, pool);

    //copy the annotatais directory item
    //type list info
    //string data item list
    //debug info
    //annotation item list
    //encodearray item list
    //annotatais directory item four bytes aligned.
    aligmentBy4Bytes(pool);
    copyOtherData(pool);

    //write the class data to file.
    copyClassData(pool);

    //copy map item.
    setMapItemOffset(pool);

    fixDexSetionOffset(pDexFile, pool);

    //fix the class offset.
    fixClassOffset(pDexFile, pool);

    //fix the item offset it the map item.
    fixAndCopyMapItemOffset(pDexFile, pool);

    //Reset the filesize, crc, and data off, data size.
    fixDexHeader(pool);

    return pool;
}

/*---dex file map-------
    header!
    string id!
    type id!
    proto id!
    field id!
    method id!
    class def id!

    annotation set ref list
    annotation set item list !

    code item !

    annotations directory item!
    type list info!
    string data item list !
    debug info!
    annotation item list !
    encodearray item list !

    class Data item !
    map list!
*/
Int32 doDex2Dex(
        DexFile* pDexFile,
        int outFd,
        long* fileLen,
        bool ifOpt,
        char const* dexfilename)
{
    UInt32 error = 0;
    D2Dpool* pool = doCopyAndFixup(pDexFile, dexfilename);
    error |= writeToFile(pool, outFd, fileLen, ifOpt);
    freePool(pool, true);
    return error;
}

Int32 doDex2Dex2(
        DexFile* pDexFile,
        unsigned char ** dxbuf,
        unsigned int* dxbuflen,
        unsigned int* cbsHandler,
        char const* dexfilename)
{
    D2Dpool* pool = doCopyAndFixup(pDexFile, dexfilename);

    *dxbuf = (unsigned char *)cbsGetData(pool->lbs);
    *dxbuflen = cbsGetSize(pool->lbs);
    *cbsHandler = pool->lbs;

    freePool(pool, false);
    return 0;
}
