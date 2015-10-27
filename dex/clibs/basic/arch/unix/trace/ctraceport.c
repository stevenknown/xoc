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

EXTERN_C void rawErrorPrint(const char* msg) {
    fprintf(stderr, "%s", msg);
    fflush(stderr);
}

EXTERN_C void rawTracePrint(const char* msg) {
    fprintf(stdout, "%s", msg);
    fflush(stdout);
}

#endif
