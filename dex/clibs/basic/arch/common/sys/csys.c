/*Copyright 2011 Alibaba Group*/

#include "sys/csys.h"
#include "sys/csysport.h"
#include "str/cstr.h"

static char _sysRootPath[512];

bool cSysInit(void) {
    return cSysInitPort();
}

void cSysFinal(void) {
    cSysFinalPort();
}
char* str_osVersion[20];
char* str_osName[40];
void cSysGetPlatform(CPlatformInfoT* info) {
    if(info) {
#if defined(__IPHONEOS__)
        cSysGetPlatformPort(info);
#elif defined(WINCE)
        info->os = "WINCE";
#elif defined(__SYMBIAN32__)
        info->os = "SYMBIAN";
#elif defined(__BREW__)
        info->os = "SYMBIAN";
#elif defined(__MTK__)
        info->os = "MTK";
#elif defined(__UNIX_LIKE__)
        info->os = "UNIX";
#elif defined(__WIN32__)
        info->os = "IPHONEOS";
        info->CPUFrequency=400000;
        info->osVersion="V1";
        info->isSupportCamera=true;
        info->isSupportGPS=true;
        info->isSupportGravity=true;
#else
        info->os = "UNKNOWN";
#endif

    }
}

void cSysSetRootPath(const char* path) {
    cStrncpy(_sysRootPath, path, 511);
}

const char* cSysGetRootPath() {
    return _sysRootPath;
}

Int32 cSysProcessorNumber()
{
    return cSysProcessorNumberPort();
}

const char** cSysGetEnvBlock(UInt32 *blockSize)
{
    return cSysGetEnvBlockPort(blockSize);
}

const char* cSysGetEnvValue(const char *name)
{
    return cSysGetEnvValuePort(name);
}

void cSysExit(Int32 status)
{
    cSysExitPort(status);
}

const struct Property *cSystemInitPlatformProperties(Int32 *num)
{
    return cSystemInitPlatformPropertiesPort(num);
}

