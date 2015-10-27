/*Copyright 2011 Alibaba Group*/
/*
 * ctrace.c
 *
 *  Created on: 2009-11-12
 *      Author: aaron
 */
#include "trace/ctraceport.h"
#include "errno/cerrno.h"
#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include "io/cioport.h"
#include "str/cstr.h"
#include "mem/cmem.h"

#ifdef MADA_DEBUG

static TraceLevelT _cTraceLevel = TRACE_LEVEL_INFO;
static Int32 _traceFd = -1;
#ifndef C_MAX_TRACE_SIZE
#define C_MAX_TRACE_SIZE (32 * 1024)
#endif

static char _cTraceBuf[C_MAX_TRACE_SIZE];

void cTraceSetFile(const char* fname) {
    if(fname != NULL && _traceFd < 0) {
        _traceFd = cIOOpenPort(fname, MADA_OF_WRITE | MADA_OF_TRUNC | MADA_OF_CREATE);
    }
}

void cTraceClose() {
    if(_traceFd >= 0) {
        cIOClosePort(_traceFd);
        _traceFd = -1;
    }
}

static void cTraceWriteFile(const char* msg, Int32 len) {
    if(_traceFd >= 0 && msg != NULL && len > 0) {
        cIOWritePort(_traceFd, msg, len);
    }
}

void cTraceSetLevel(TraceLevelT level) {
    if(level > TRACE_LEVEL_NONE)
        level = TRACE_LEVEL_NONE;

    _cTraceLevel = level;
}


bool cTraceOutV(TraceLevelT level, bool line,  const char* fmt, va_list vlist) {
    Int32 len;

    if(level < _cTraceLevel)
        return true;

    if(fmt == NULL) {
        _cerrno = ERROR_MEM_NULL_OBJ;
        return false;
    }
    cMemset(_cTraceBuf, 0, C_MAX_TRACE_SIZE);

    cVsnprintf(_cTraceBuf, C_MAX_TRACE_SIZE-2, fmt, vlist);

    len = cStrlen(_cTraceBuf);
    if(line && (len == 0 || _cTraceBuf[len-1] != '\n')) {
        _cTraceBuf[len] = '\n';
        _cTraceBuf[++len] = 0;
    }

    if(level >= TRACE_LEVEL_ERROR) {
        rawErrorPrint(_cTraceBuf);
    }
    else {
        rawTracePrint(_cTraceBuf);
    }

    va_end(vlist);

    cTraceWriteFile(_cTraceBuf, len);
    return true;
}

bool cTraceOut(TraceLevelT level, const char* fmt, ...) {
    va_list vlist;

    va_start(vlist, fmt);
    cTraceOutV(level, false, fmt, vlist);
    va_end(vlist);
    return true;
}

bool cTrace(const char* fmt, ...) {
    va_list vlist;

    va_start(vlist, fmt);
    cTraceOutV(TRACE_LEVEL_INFO, false, fmt, vlist);
    va_end(vlist);
    return true;
}

bool cTraceMsg(const char* msg) {
    Int32 len;

    if(TRACE_LEVEL_INFO < _cTraceLevel)
        return true;

    if(msg == NULL) {
        _cerrno = ERROR_MEM_NULL_OBJ;
        return false;
    }

    len = cStrlen(msg);

    rawTracePrint(msg);

    cTraceWriteFile(msg, len);
    return true;
}

bool cTraceLine(const char* msg) {
    Int32 len;

    if(TRACE_LEVEL_INFO < _cTraceLevel)
        return true;

    if(msg == NULL) {
        _cerrno = ERROR_MEM_NULL_OBJ;
        return false;
    }
    cMemset(_cTraceBuf, 0, C_MAX_TRACE_SIZE);

    cStrncpy(_cTraceBuf, msg, C_MAX_TRACE_SIZE-2);

    len = cStrlen(_cTraceBuf);
    if(len == 0 || _cTraceBuf[len-1] != '\n') {
        _cTraceBuf[len] = '\n';
        _cTraceBuf[++len] = 0;
    }

    rawTracePrint(_cTraceBuf);

    cTraceWriteFile(_cTraceBuf, len);
    return true;
}

#endif

