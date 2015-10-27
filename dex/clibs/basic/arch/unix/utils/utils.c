/*Copyright 2011 Alibaba Group*/
/*
 *  win32util.c
 *
 *  aaron 04/28/2009
 *
 */

#include "std/cstd.h"
#include "trace/ctraceport.h"
#include "io/cioport.h"
#include <unistd.h>
#include <stdlib.h>
#include <string.h>

/*
UInt32 UTF8ToANSI(const char* src, UInt32 cbSrc, char* dst, UInt32 cbDst) {
    int ret, cbtmp;
    WCHAR* tmp = NULL;
    if(src == NULL || cbSrc < 0 || dst == NULL || cbDst < 0)
        return 0;

    ret = MultiByteToWideChar(CP_UTF8, 0, src, (int)cbSrc, NULL,0);
    if(ret == 0)
        return 0;

    cbtmp = ret;
    tmp = (WCHAR*)malloc(cbtmp * 2);
    if(tmp == NULL)
        return 0;

    ret = MultiByteToWideChar(CP_UTF8, 0, src, (int)cbSrc, tmp, cbtmp);
    if(ret == 0)
        return 0;

    ret = WideCharToMultiByte(CP_ACP,0,tmp,cbtmp,dst, (int)cbDst,NULL,NULL);

    if(tmp != NULL)
        free(tmp);

    return (UInt32)(ret-1);
}

UInt32 ANSIToUTF8(const char* src, UInt32 cbSrc, char* dst, UInt32 cbDst) {
    int ret, cbtmp;
    WCHAR* tmp = NULL;
    if(src == NULL || cbSrc < 0 || dst == NULL || cbDst < 0)
        return 0;

    ret = MultiByteToWideChar(CP_ACP, 0, src, (int)cbSrc, NULL,0);
    if(ret == 0)
        return 0;

    cbtmp = ret;
    tmp = (WCHAR*)malloc(cbtmp * 2);
    if(tmp == NULL)
        return 0;

    ret = MultiByteToWideChar(CP_ACP, 0, src, (int)cbSrc, tmp, cbtmp);
    if(ret == 0)
        return 0;

    ret = WideCharToMultiByte(CP_UTF8,0,tmp,cbtmp,dst, (int)cbDst,NULL,NULL);

    if(tmp != NULL)
        free(tmp);

    return (UInt32)(ret);
}


*/
char _madaRootPath[1024];

const char* madaGetRootPath(void) {
    return _madaRootPath;
}

/*
static void replaceFilePath(char* path) {
    int i, len;
    if(path == NULL)
        return;
    len = strlen(path);
    for(i = 0; i < len; i++) {
        if(path[i] == '\\') {
            path[i] = '/';
        }
    }
}

static bool checkRootPath(char* buff) {
    char* p;
    char tmp[512];
    int maxLevel, i , len;
    len = strlen(buff);


    maxLevel = 4;

    strcpy(tmp, buff);
    p = tmp+len-1;
    if(*p == '/') {
        *p = 0;
        p--;
    }

    for(i = 0; i < maxLevel; i++) {
        strcat(tmp, "/root/etc");
        if(cIOExistsPort(tmp)) {
            p[1] = 0;
            strcpy(buff, tmp);
            return true;
        }
        else {
            while(*p != '/') {
                p--;
            }
            *p = 0;
            p--;
        }
        if(p <= tmp)
            break;
    }

    strcpy(tmp, buff);
    p = tmp+len-1;
    if(*p == '/') {
        *p = 0;
        p--;
    }

    for(i = 0; i < maxLevel; i++) {
        strcat(tmp, "/projects/root/etc");
        if(cIOExistsPort(tmp)) {
            p[1+9] = 0;
            strcpy(buff, tmp);
            return true;
        }
        else {
            cTrace("%s not exist\n", tmp);
            while(*p != '/') {
                p--;
            }
            *p = 0;
            p--;
        }
        if(p <= tmp)
            break;
    }
    return false;
}

*/
bool initMadaRootPath(void) {

    if (!getcwd(_madaRootPath, sizeof _madaRootPath)) {
        perror("initMadaRootPath");
        exit(EXIT_FAILURE);
        return false;
    }
    return true;

/*
    char buff[1024];
    char utf8Buff[256*3];

    memset(_madaRootPath, 0, sizeof(_madaRootPath));
    memset(buff, 0, sizeof(buff));
    _getcwd(buff, sizeof( buff));
    replaceFilePath(buff);
    cTrace("work dir=%s\n", buff);
    if(!checkRootPath(buff)) {
        cTrace("check root failed: %s\n", buff);
        return false;
    }

    memset(utf8Buff, 0, sizeof(utf8Buff));
    ANSIToUTF8(buff, strlen(buff), utf8Buff, sizeof(utf8Buff));
    strcpy(_madaRootPath, utf8Buff);
    strcat(_madaRootPath, "/root");

    return true;
*/
}

/*

bool initKniPath(void) {
    char buff[1024];
    char utf8Buff[256*3];
    char* p;
    HWND hwnd;

    memset(_madaRootPath, 0, sizeof(_madaRootPath));
    memset(buff, 0, sizeof(buff));

    hwnd = GetModuleHandle("kni.dll");

    GetModuleFileName(hwnd, buff, sizeof(buff));

    //_getcwd(buff, sizeof( buff));
    replaceFilePath(buff);
    cTrace("work dir=%s\n", buff);
    p = strstr(buff, "/root/bin");
    if(p != NULL) {
        *p = 0;
        strcat(buff, "/root");
    }
    else {
        _getcwd(buff, sizeof( buff));
        replaceFilePath(buff);
        p = strstr(buff, "/root/lib");
        if(p != NULL) {
            *p = 0;
            strcat(buff, "/root");
        }
        else {

        }
    }

    memset(utf8Buff, 0, sizeof(utf8Buff));
    ANSIToUTF8(buff, strlen(buff), utf8Buff, sizeof(utf8Buff));
    strcpy(_madaRootPath, utf8Buff);

    return true;
}
*/

