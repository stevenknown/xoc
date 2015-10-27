/*Copyright 2011 Alibaba Group*/
/*
 * cthread.c
 *
 *  Created on: Apr 17, 2009
 *      Author: misa
 */

#include "thread/cthreadport.h"

void cMutexInit(MutexT * mt) {
    cMutexInitPort(mt);
}

void cMutexLock(MutexT * mt) {
    cMutexLockPort(mt);
}

void cMutexUnlock(MutexT * mt) {
    cMutexUnlockPort(mt);
}

void cMutexDestroy(MutexT *mt) {
    cMutexDestroyPort(mt);
}

void cSleep(Int32 ms) {
    cSleepPort(ms);
}
