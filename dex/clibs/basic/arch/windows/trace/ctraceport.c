/*Copyright 2011 Alibaba Group*/
/*
 *  ctraceport.c
 *  madagascar
 *
 *  Created by Misa.Z on 2/19/09.
 *  Copyright 2009 Virtualnology. All rights reserved.
 *
 */

#include "trace/ctraceport.h"
#include <stdarg.h>
#include <stdio.h>
#include <string.h>

#ifdef MADA_DEBUG

extern int UTF8ToANSI(const char* src,int cbSrc, char *dst, int cbDst);
static char _ansiBuffer[C_MAX_TRACE_SIZE];

EXTERN_C void rawErrorPrint(const char* msg) {
    memset(_ansiBuffer, 0, sizeof(_ansiBuffer));
    UTF8ToANSI(msg, strlen(msg), _ansiBuffer, sizeof(_ansiBuffer)-1);
    fprintf(stderr, "%s", _ansiBuffer);
    fflush(stderr);
}

EXTERN_C void rawTracePrint(const char* msg) {
    memset(_ansiBuffer, 0, sizeof(_ansiBuffer));
    UTF8ToANSI(msg, strlen(msg), _ansiBuffer, sizeof(_ansiBuffer)-1);
    fprintf(stdout, "%s", _ansiBuffer);
    fflush(stdout);
}

#endif



