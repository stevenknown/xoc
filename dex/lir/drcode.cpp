/*@
XOC Release License

Copyright (c) 2013-2014, Alibaba Group, All rights reserved.

    compiler@aliexpress.com

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

THIS SOFTWARE IS PROVIDED "AS IS" AND ANY EXPRESS OR IMPLIED
WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS
BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

author: GongKai, JinYue
@*/
#include "libdex/DexFile.h"
#include "libdex/DexClass.h"
#include "libdex/DexCatch.h"
#include "libdex/InstrUtils.h"
#include "libdex/DexProto.h"
#include "libdex/CmdUtils.h"

#include "dir.h"
#include "liropcode.h"
#include <assert.h>
#include <stdio.h>

#include "xassert.h"
#include "lir.h"
#include "d2d_comm.h"
#include "d2d_l2d.h"
#include "d2d_d2l.h"
#include "cominc.h"
#include "comopt.h"
#include "drAlloc.h"
#include "utils/cbytestream.h"
#include "dex.h"
#include "gra.h"
#include "dex_hook.h"
#include "dex_util.h"
#include "dex_driver.h"
#include "drcode.h"

Int32 gMemAlloc = 0;

typedef struct {
    UInt32 posNum;
    UInt32* posMap;
} PositionMap;

static bool isObjectInit(const DexFile* pDexFile, DIRDecodedInsn* pDecInsn)
{
    const DexMethodId* pMethodId;
    u4 methodIdx;
    const char* className;

    if (pDecInsn->opcode != OP_INVOKE_DIRECT)
        return false;

    methodIdx = pDecInsn->vB;
    pMethodId = dexGetMethodId(pDexFile, methodIdx);
    className = dexStringByTypeIdx(pDexFile,pMethodId->classIdx);
    if (strcmp("Ljava/lang/Object;",className) == 0) {
        const char* methodName;

        methodName = dexStringById(pDexFile,pMethodId->nameIdx);
        if (strcmp("<init>",methodName) == 0)
            return true;
    }

    return false;
}

static inline bool contentIsInsn(const UInt16 *codePtr)
{
    UInt16 instr = *codePtr;
    DIROpcode opcode = (DIROpcode)(instr & 0xff);
    // if it is pseudo code: switch-data, return true
    if (instr == 0x100 || instr == 0x200) {
        return true;
    }

    return (opcode != 0 || instr == 0);
}

static inline DIROpcode getOpcodeFromCodeUnit(UInt16 codeUnit)
{
    int lowByte = codeUnit & 0xff;
    if (codeUnit == 0x100)
        return 227;
    if (codeUnit == 0x200)
        return 228;
    if (lowByte != 0xff) {
        return (DIROpcode) lowByte;
    } else {
        return (DIROpcode) ((codeUnit >> 8) | 0x100);
    }
}

Int32 findPos(PositionMap* posMap,UInt32 target)
{
    UInt32 low;
    UInt32 mid;
    UInt32 high;
    UInt32 tmp;
    UInt32* map = posMap->posMap;
    UInt32  num = posMap->posNum;

    low = 0;
    high = num;
    while (true) {
        mid = (low + high) >> 1;
        tmp = map[mid];
        if (tmp == target) {
            return mid;
        }
        if (mid == low)
            break;
        if (target < tmp)
            high = mid;
        else
            low = mid;
    }

    UNREACHABLE();
    return -1;
}

void genInstruction(
        const DexFile* pDexFile,
        UInt16 *codeStart,
        UInt16 *codeEnd,
        LIRBaseOp** lirList,
        PositionMap* posMap)
{
    UInt32 instrIdx = 0;
    UInt16* codePtr = codeStart;

    UInt32 dexOffset = 0;

    while (codePtr < codeEnd) {
        LIRBaseOp* result;

        if (!contentIsInsn(codePtr)) {
            break;
        }
        UInt16 instr = *codePtr;
        DIROpcode opcode = getOpcodeFromCodeUnit(instr);
        UInt8 lirOpcode = gDIR2LIRInfo.opcodes[opcode];
        UInt8 flags = gDIR2LIRInfo.flags[opcode];
        UInt32 formats = gLIROpcodeInfo.formats[lirOpcode];
        DIRDecodedInsn dInsn;
        ::memset(&dInsn,0,sizeof(DIRDecodedInsn));
        DIRDecodeInstruction(codePtr,&dInsn);

        switch(formats) {
        case lirFmt00X:
        {
            LIRBaseOp* lir =  (LIRBaseOp*)LIRMALLOC(sizeof(LIRBaseOp));
            lir->opcode = lirOpcode;
            lir->flags = flags;

            result = (LIRBaseOp*)lir;
            break;
        }
        case lirFmtV:
        {
            LIRBaseOp* lir =  (LIRBaseOp*)LIRMALLOC(sizeof(LIRBaseOp));
            lir->opcode = lirOpcode;
            lir->flags = flags;

            result = (LIRBaseOp*)lir;
            break;
        }
        case lirFmtA:
        case lirFmtR:
        {
            LIRAOp* lir = (LIRAOp*)LIRMALLOC(sizeof(LIRAOp));
            lir->opcode = lirOpcode;
            lir->flags = flags;
            //value or dst
            lir->vA = dInsn.vA;

            result = (LIRBaseOp*)lir;
            break;
        }
        case lirFmtT://goto
        {
            LIRGOTOOp* lir = (LIRGOTOOp*)LIRMALLOC(sizeof(LIRGOTOOp));
            lir->opcode = lirOpcode;
            lir->flags = flags;

            UInt32 target;
            //value or dst
            if (opcode == OP_GOTO) {
                target = dexOffset +((Int8)dInsn.vA);
            } else if (opcode == OP_GOTO_16) {
                target = dexOffset +((Int16)dInsn.vA);
            } else{
                target = dexOffset +((Int32)dInsn.vA);
            }
            Int32 instrNum = findPos(posMap,target);
            lir->target = instrNum;

            result = (LIRBaseOp*)lir;
            break;
        }
        case lirFmtRA:
        {
            LIRABOp* lir = (LIRABOp*)LIRMALLOC(sizeof(LIRABOp));
            lir->opcode = lirOpcode;
            lir->flags = flags;
            //value or dst
            lir->vA = dInsn.vA;
            lir->vB = dInsn.vB;

            result = (LIRBaseOp*)lir;
            break;
        }
        case lirFmtRS:
        case lirFmtAS:
        {
            LIRABOp* lir = (LIRABOp*)LIRMALLOC(sizeof(LIRABOp));
            lir->opcode = lirOpcode;
            lir->flags = flags;
            //value or dst
            lir->vA = dInsn.vA;
            lir->vB = dInsn.vB;

            result = (LIRBaseOp*)lir;
            break;
        }
        case lirFmtAT:
        {
            LIRABOp* lir = (LIRABOp*)LIRMALLOC(sizeof(LIRABOp));
            lir->opcode = lirOpcode;
            lir->flags = flags;
            //value or dst
            lir->vA = dInsn.vA;
            UInt32 target = dexOffset +((Int16)dInsn.vB);
            Int32 instrNum = findPos(posMap,target);
            lir->vB = instrNum;

            result = (LIRBaseOp*)lir;
            break;
        }
        case lirFmtRL://LOP_CONST
        {
            LIRConstOp* lir = (LIRConstOp*)LIRMALLOC(sizeof(LIRConstOp));
            lir->opcode = lirOpcode;
            lir->flags = flags;
            //value or dst
            lir->vA = dInsn.vA;

            if (opcode == OP_CONST_HIGH16) {
                lir->vB = (UInt32)(dInsn.vB << 16);
            } else if (opcode == OP_CONST_WIDE_HIGH16) {
                Int64 data = (Int16)dInsn.vB;
                lir->vB = (Int64)(data << 48);
            } else if (opcode == OP_CONST_WIDE) {
                lir->vB = dInsn.vB_wide;
            } else if (flags == LIR_JDT_wide) {
                lir->vB = (Int32)dInsn.vB;
            } else{
                lir->vB = (UInt32)dInsn.vB;
            }

            result = (LIRBaseOp*)lir;
            break;
        }
        case lirFmtABC:
        case lirFmtRAB:
        case lirFmtRAL:
        {
            LIRABCOp* lir = (LIRABCOp*)LIRMALLOC(sizeof(LIRABCOp));
            lir->opcode = lirOpcode;
            lir->flags = flags;
            //value or dst
            lir->vA = dInsn.vA;
            lir->vB = dInsn.vB;
            lir->vC = dInsn.vC;

            result = (LIRBaseOp*)lir;
            break;
        }
        case lirFmtABS:
        case lirFmtRAS:
        {
            LIRABCOp* lir = (LIRABCOp*)LIRMALLOC(sizeof(LIRABCOp));
            lir->opcode = lirOpcode;
            lir->flags = flags;
            //value or dst
            lir->vA = dInsn.vA;
            lir->vB = dInsn.vB;
            lir->vC = dInsn.vC;
            result = (LIRBaseOp*)lir;
            break;
        }
        case lirFmtABT:
        {
            LIRABCOp* lir = (LIRABCOp*)LIRMALLOC(sizeof(LIRABCOp));
            lir->opcode = lirOpcode;
            lir->flags = flags;
            //value or dst
            lir->vA = dInsn.vA;
            lir->vB = dInsn.vB;

            UInt32 target = dexOffset +((Int16)dInsn.vC);
            Int32 instrNum = findPos(posMap,target);
            lir->vC = instrNum;

            result = (LIRBaseOp*)lir;
            break;
        }
        case lirFmtSWITCH:
        {
            UInt32 dataSize;
            UInt16* data;

            LIRSwitchOp* lir = (LIRSwitchOp*)LIRMALLOC(sizeof(LIRSwitchOp));
            lir->opcode = lirOpcode;
            lir->flags = flags;
            lir->value = dInsn.vA;

            data = codePtr + dInsn.vB;

            switch(lirOpcode) {
            case LOP_TABLE_SWITCH:
            {
                assert(data[0] == 0x0100);
                UInt32 i;
                Int32* target;
                UInt32* lirTarget;
                UInt32 size = data[1];

                dataSize = 8 + size * 4;
                lir->data = (UInt16*)LIRMALLOC(dataSize);
                ::memcpy(lir->data,(BYTE*)data,8);

                target = (Int32*)(((BYTE*)data) + 8);
                lirTarget = (UInt32*)(((BYTE*)lir->data) + 8);
                for(i = 0; i < size; i++) {
                    UInt32 addr = dexOffset + (Int32)target[i];
                    Int32 instrNum = findPos(posMap,addr);
                    lirTarget[i] = (UInt32)instrNum;
                }
                break;
            }
            case LOP_LOOKUP_SWITCH:
            {
                assert(data[0] == 0x0200);
                UInt32 i,copySize;
                Int32 *target;
                UInt32 *lirTarget;
                UInt32 size = data[1];

                dataSize = 4 + size * 8;
                lir->data = (UInt16*)LIRMALLOC(dataSize);
                copySize = 4 + size*4;
                ::memcpy(lir->data,(BYTE*)data,copySize);

                target = (Int32*)(((BYTE*)data) + copySize);
                lirTarget = (UInt32*)(((BYTE*)lir->data) + copySize);
                for(i = 0; i < size; i++) {
                    UInt32 addr = dexOffset + (Int32)target[i];
                    Int32 instrNum = findPos(posMap,addr);
                    lirTarget[i] = (UInt32)instrNum;
                }

                break;
            }
            case LOP_FILL_ARRAY_DATA:
            {
                assert(data[0] == 0x0300);
                UInt16 elemWidth = data[1];
                UInt32 len = data[2] | (((UInt32)data[3]) << 16);
                dataSize = (4 + (elemWidth * len + 1)/2) * 2;
                lir->data = (UInt16*)LIRMALLOC(dataSize);
                ::memcpy(lir->data,(BYTE*)data,dataSize);
                break;
            }
            default: UNREACHABLE();
            }
            result = (LIRBaseOp*)lir;
            break;
        }
        case lirFmtINVOKE:
        {
            LIRInvokeOp* lir = (LIRInvokeOp*)LIRMALLOC(sizeof(LIRInvokeOp));
            const DexMethodId* method = NULL;
            const DexProtoId* proto = NULL;
            const char* shorty = NULL;
            lir->flags = flags;

            /* TODO: to process the objectinit and excute inline
            if (isObjectInit(pDexFile, &dInsn)) {
                lir->opcode = LOP_NOP;
                goto END;
            } else {
                Int32 idx = isExecuteInlineMethod(pDexFile, (DecodedInstruction*)(&dInsn));
                if (idx != -1)
                {
                    lir->flags = flags | LIR_invoke_inline;
                    lir->exeRef = idx;
                }
                else
                    lir->flags = flags;
            }
            */

            lir->method = (void*)dInsn.vB;
            lir->argc = dInsn.vA;
            lir->opcode = lirOpcode;

            method = dexGetMethodId(pDexFile, (UInt32)(ULong)lir->method);
            proto = dexGetProtoId(pDexFile, method->protoIdx);
            shorty = strdup(dexStringById(pDexFile, proto->shortyIdx));
            lir->shorty = shorty;
            if (dInsn.vA != 0) {
                lir->args = (UInt16*)LIRMALLOC(dInsn.vA * sizeof(UInt32));
            } else {
                lir->args = NULL;
            }

           if ((flags & 0xf0) == LIR_Range) {
                int i;
                for(i = 0; i < lir->argc; i++) {
                    lir->args[i] =     dInsn.vC + i;
                }
            } else{
                int i;
                for(i = 0; i < lir->argc; i++) {
                    lir->args[i] =     dInsn.arg[i];
                }
            }
END:
            result = (LIRBaseOp*)lir;
            break;
        }default:{
            result = NULL;
            UNREACHABLE();
        }
        }

        // if we happen to 0x100/200, add the codePtr+width, then continue
        // to find next instruction.
        // Calculate the width for 0x100/0x200
        // if 0x100, width = size * 2 + 4
        // if 0x200, width = size * 4 + 2
        // size is data[1]
        if (formats == lirFmt00X) {
            UInt32 width;
            switch (instr) {
                case 0x100: {
                    UInt16* data = codePtr;
                    UInt32 size = data[1];
                    width = size * 2 + 4;
                    break;
                }
                case 0x200: {
                    UInt16* data = codePtr;
                    UInt32 size = data[1];
                    width = size * 4 + 2;
                    break;
                }
                default: UNREACHABLE();
                    width = gDIROpcodeInfo.widths[opcode];
                    break;
            }
            codePtr += width;
            dexOffset += width;
            continue;
        }

        lirList[instrIdx] = result;
        UInt32 width = gDIROpcodeInfo.widths[opcode];
        codePtr += width;
        dexOffset += width;
        instrIdx ++;
    }
}

static void genTryCatches(
        const DexFile* pDexFile,
        const DexCode* dexCode,
        PositionMap* posMap,
        LIRCode* code)
{
    const DexTry* pTries = dexGetTries(dexCode);
    UInt32 triesSize = dexCode->triesSize;

    code->triesSize = triesSize;
    code->trys = NULL;
    if (triesSize == 0) {
        return;
    }

    LIROpcodeTry* trys = (LIROpcodeTry*)LIRMALLOC(triesSize*sizeof(LIROpcodeTry));
    code->trys = trys;

    for (UInt32 i = 0; i < triesSize; i++) {
        const DexTry* pTry = &pTries[i];
        UInt32 start = pTry->startAddr;
        UInt32 end = start + pTry->insnCount;
        UInt32 newStart;
        UInt32 newEnd;
        UInt32 handlerSize;
        DexCatchIterator iterator;

        newStart = findPos(posMap,start);
        newEnd = findPos(posMap,end);

        LIROpcodeTry* _try = trys + i;

        _try->start_pc = newStart;
        _try->end_pc = newEnd;

        dexCatchIteratorInit(&iterator, dexCode, pTry->handlerOff);
        handlerSize = 0;
        while (true) {
            DexCatchHandler* handler = dexCatchIteratorNext(&iterator);
            if (handler == NULL) {
                break;
            }
            handlerSize ++;
        }

        _try->catchSize = handlerSize;
        _try->catches =
            (LIROpcodeCatch*)LIRMALLOC(handlerSize*sizeof(LIROpcodeCatch));

        dexCatchIteratorInit(&iterator, dexCode, pTry->handlerOff);
        UInt32 idx = 0;
        while (true) {
            DexCatchHandler* handler = dexCatchIteratorNext(&iterator);
            UInt32 newHandler;

            if (handler == NULL) {
                break;
            }

            LIROpcodeCatch* _catch = _try->catches + idx;
            newHandler = findPos(posMap,handler->address);

            _catch->handler_pc = newHandler;

            if (handler->typeIdx == kDexNoIndex) {
                _catch->class_type = 0x00;  //TODO may a bug
            } else{
                _catch->class_type = (UInt32)handler->typeIdx;
            }
            idx++;
        }
    }
}

//Obsolete code.
//#ifdef __cplusplus
//extern "C" {
//#endif
//    LIRCode* anaEntry(LIRCode* lirCode);
//#ifdef __cplusplus
//}
//#endif
//astatic Int32 l2dWithAot(D2Dpool* pool, const DexCode* dexCode, LIRCode* code)
//{
//   Int32 err = 0;
//    code = anaEntry(code);
//    lir2dexCode(pool, dexCode, code);
//    return err;
//}

#ifdef COMPILE_DEX2LEX
bool aotDrGenCode(
        const DexFile* pDexFile,
        DexMethod* pDexMethod,
        LCodeData* codeData)
{
    const DexCode* dexCode = dexGetCode(pDexFile, pDexMethod);
    UInt16* codeStart = (UInt16*)dexCode->insns;
    UInt16* codeEnd = codeStart + dexCode->insnsSize;
    UInt16* codePtr = codeStart;

    /*init mem heap*/
    drLinearInit();

    LIRCode* lircode = (LIRCode*)LIRMALLOC(sizeof(LIRCode));

    //const DexMethodId* method = dexGetMethodId(pDexFile, pDexMethod->methodIdx);
     LIRBaseOp** lirList;

    UInt32 instrCount = 0;
    PositionMap positionMap;

    /*positionMap may bigger than we need.*/
    positionMap.posMap =
        (UInt32*)::malloc((codeEnd - codeStart + 1) * sizeof(UInt32));

    UInt32 dexOffset = 0;
    while (codePtr < codeEnd) {
        if (!contentIsInsn(codePtr)) {
            break;
        }
        UInt16 instr = *codePtr;
        DIROpcode opcode = getOpcodeFromCodeUnit(instr);
        UInt32 width = gDIROpcodeInfo.widths[opcode];

        positionMap.posMap[instrCount] = dexOffset;

        codePtr += width;
        dexOffset += width;

        instrCount++;
    }

    positionMap.posMap[instrCount] = dexOffset;
    positionMap.posNum = instrCount + 1;

    lirList = (LIRBaseOp**)LIRMALLOC(instrCount * sizeof(LIRBaseOp*));

    ::memset(lirList, 0 ,instrCount*sizeof(LIRBaseOp*));

    lircode->instrCount = instrCount;
    lircode->lirList = lirList;
    lircode->maxVars = dexCode->registersSize;

    //gen instruction
    genInstruction(pDexFile, codeStart, codeEnd, lirList, &positionMap);

    //ge try catch
    genTryCatches(pDexFile, dexCode, &positionMap, lircode);

    free(positionMap.posMap);

    const DexMethodId* pMethodId;
    pMethodId = dexGetMethodId(pDexFile, pDexMethod->methodIdx);

    lircode->numArgs = dexCode->insSize;
    lircode->flags = 0;

    //tmp impl
    lircode->strClass = strdup(dexStringByTypeIdx(pDexFile, pMethodId->classIdx));
    lircode->strName = strdup(dexStringById(pDexFile, pMethodId->nameIdx));

    const DexProtoId* proto = dexGetProtoId(pDexFile, pMethodId->protoIdx);
    const char* shorty = strdup(dexStringById(pDexFile, proto->shortyIdx));
    lircode->shortName = shorty;

    if ((ACC_STATIC & pDexMethod->accessFlags))
        lircode->flags |= LIR_FLAGS_ISSTATIC;

    /*analyse the lir to make it better,
     *and transform lir to lex*/
    lir2lexTransform(lircode, codeData);

    /*free heap*/
    drLinearFree();
    return true;
}
#endif


//Only used on debug mode and single thread, because the tricky usage
//might lead to string allocated in this function overrided by other thread.
CHAR const* debugAssemblyName(DexFile const* df, DexMethod const* dexm)
{
    CHAR tmp[256];
    CHAR * name = NULL;
    CHAR const* classname = get_class_name(df, dexm);
    CHAR const* funcname = get_func_name(df, dexm);
    CHAR const* functype = get_func_type(df, dexm);
    UINT len = strlen(classname) + strlen(funcname) + strlen(functype) + 10;

    if (len < 256) { name = tmp; }
    else {
        name = (CHAR*)ALLOCA(len);
        ASSERT0(name);
    }

    //Function string is consist of these.
    assemblyUniqueName(name, classname, functype, funcname);
    return name;
}

bool checkMethodName(DexFile const* df, DexMethod const* dexm)
{
    CHAR tmp[256];
    CHAR * runame = NULL;
    CHAR const* classname = get_class_name(df, dexm);
    CHAR const* funcname = get_func_name(df, dexm);
    CHAR const* functype = get_func_type(df, dexm);
    UINT len = strlen(classname) + strlen(funcname) + strlen(functype) + 10;

    if (len < 256) { runame = tmp; }
    else {
        runame = (CHAR*)ALLOCA(len);
        ASSERT0(runame);
    }

    //Function string is consist of these.
    assemblyUniqueName(runame, classname, functype, funcname);

    if (strcmp(runame,  "Landroid/openapi/v2/AdInfo;:()V:<init>")!=0) {
        return true;
    }
    return false;
}

void d2rMethod(
        D2Dpool* pool,
        DexFile* pDexFile,
        const DexMethod* pDexMethod,
        const DexClassDef* classdef,
        RegionMgr* rumgr,
        List<DexRegion const*> * rulist)
{
    const DexCode* dexCode = dexGetCode(pDexFile, pDexMethod);
    UInt16* codeStart = (UInt16*)dexCode->insns;
    UInt16* codeEnd = codeStart + dexCode->insnsSize;
    UInt16* codePtr = codeStart;

    //Only for debug.
    //if (checkMethodName(pDexFile, pDexMethod)) { return true; }
    #ifdef _DEBUG_
    CHAR const* namestr = debugAssemblyName(pDexFile, pDexMethod);
    #endif

    drLinearInit();

    LIRCode* lircode = (LIRCode*)LIRMALLOC(sizeof(LIRCode));
    LIRBaseOp** lirList;

    UInt32 instrCount = 0;

    //positionMap may bigger than we need.
    UInt32 insnum = codeEnd - codeStart + 1;

    PositionMap positionMap;

    positionMap.posMap = (UInt32*)::malloc(insnum * sizeof(UInt32));
    UInt32 dexOffset = 0;
    UInt32 lastValidDexOffset = 0;
    bool lastInstrIsPseudo = false;
    OffsetVec offvec;
    while (codePtr < codeEnd) {
        if (!contentIsInsn(codePtr)) {
            break;
        }

        //Record the Dex instruction half-word offset.
        offvec.set(instrCount, dexOffset);

        UInt16 instr = *codePtr;
        if (instr == 0x100 || instr == 0x200) {
            // If the prev insn is not pseudo, update the lastValidDexOffset.
            if (!lastInstrIsPseudo) {
                lastValidDexOffset = dexOffset;
            }

            lastInstrIsPseudo = true;
            UInt32 width = 0;
            switch (instr) {
                case 0x100: {
                    UInt16* data = codePtr;
                    UInt32 size = data[1];
                    width = size * 2 + 4;
                    break;
                }
                case 0x200: {
                    UInt16* data = codePtr;
                    UInt32 size = data[1];
                    width = size * 4 + 2;
                    break;
                }
                default: UNREACHABLE();
            }
            codePtr += width;
            dexOffset += width;
        } else {
            //If insn is not pseudo insn, update posMap;
            //otherwise, continue to the next insn.
            lastInstrIsPseudo = false;
            DIROpcode opcode = getOpcodeFromCodeUnit(instr);
            UInt32 width = gDIROpcodeInfo.widths[opcode];

            positionMap.posMap[instrCount] = dexOffset;
            ASSERTN(instrCount < insnum,
                   ("instrCount overflowed, may be the DEX file is encrypted"));

            codePtr += width;
            dexOffset += width;
            instrCount++;
        }
    }

    ASSERT0(instrCount < insnum);

    //If the last instr is pseudo, the valid dexoffset is the
    //lastValidDexOffset but not the dexoffset.
    if (lastInstrIsPseudo) {
        positionMap.posMap[instrCount] = lastValidDexOffset;
    } else {
        positionMap.posMap[instrCount] = dexOffset;
    }
    positionMap.posNum = instrCount + 1;

    lirList = (LIRBaseOp**)LIRMALLOC(instrCount * sizeof(LIRBaseOp*));
    ::memset(lirList, 0, instrCount * sizeof(LIRBaseOp*));

    lircode->instrCount = instrCount;
    lircode->lirList = lirList;
    lircode->maxVars = dexCode->registersSize;

    const DexMethodId* pMethodId;
    pMethodId = dexGetMethodId(pDexFile, pDexMethod->methodIdx);

    lircode->numArgs = dexCode->insSize;
    lircode->flags = 0;

    //tmp impl
    lircode->strClass = dexStringByTypeIdx(pDexFile, pMethodId->classIdx);
    lircode->strName = dexStringById(pDexFile, pMethodId->nameIdx);

    const DexProtoId* proto = dexGetProtoId(pDexFile, pMethodId->protoIdx);
    const char* shorty = dexStringById(pDexFile, proto->shortyIdx);
    lircode->shortName = shorty;

    //gen instruction
    genInstruction(pDexFile, codeStart, codeEnd, lirList, &positionMap);

    //ge try catch
    genTryCatches(pDexFile, dexCode, &positionMap, lircode);

    free(positionMap.posMap);

    if ((ACC_STATIC & pDexMethod->accessFlags)) {
        lircode->flags |= LIR_FLAGS_ISSTATIC;
    }

    compileFunc(rumgr, pool, lircode, pDexFile,
                pDexMethod, dexCode, classdef, offvec, rulist);
    lir2dexCode(pool, dexCode, lircode);

    //Leave it to verify.
    //lir2dexCode_orig(pool, dexCode, code);

    //Obsolete code.
    //l2dWithAot(pool, dexCode, code);
    drLinearFree();
}
