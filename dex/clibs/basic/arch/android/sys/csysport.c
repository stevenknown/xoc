/*Copyright 2011 Alibaba Group*/
/*
 *  cesysport.c
 *  madagascar
 *
 *  Created by Misa.Z on 2/24/09.
 *  Copyright 2009 Virtualnology. All rights reserved.
 *
 */

#include "sys/csysport.h"
#include <string.h>
//#include <SDL/SDL.h>
#include <unistd.h>

bool cSysInitPort(void) {
    //SDL_Init(SDL_INIT_VIDEO);
    return true;
}

void cSysFinalPort(void) {
    //SDL_Quit();
}

Int32 cSysProcessorNumberPort()
{
    return sysconf(_SC_NPROCESSORS_CONF);
}


const char** cSysGetEnvBlockPort(UInt32 *blockSize)
{
    //TODO
    return NULL;
}

const char* cSysGetEnvValuePort(const char *name)
{
    //TODO
    return NULL;
}

void cSysExitPort(Int32 status)
{
    exit(status);
}

const struct Property *cSystemInitPlatformPropertiesPort(int *num)
{
    *num = 0;
    return NULL;
}
