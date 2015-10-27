/*Copyright 2011 Alibaba Group*/
/*
 * cthreadport.c
 *
 *  Created on: Apr 17, 2009
 *      Author: misa
 */

#include "thread/cthreadport.h"
#include "mem/cmemport.h"
#include <pthread.h>
#include <unistd.h>

static pthread_mutexattr_t g_mutex_attr;

void cSleepPort(int ms) {
    usleep(ms*1000);
}

void cMutexInitPort(MutexT * mt) {
    int ret;
    if(mt) {
        pthread_mutexattr_init(&g_mutex_attr);
        pthread_mutexattr_settype(&g_mutex_attr, PTHREAD_MUTEX_RECURSIVE);

        mt->mutex = (void*)cMalloc(sizeof(pthread_mutex_t));
        pthread_mutex_init((pthread_mutex_t*)mt->mutex, &g_mutex_attr);
    }
}

void cMutexLockPort(MutexT * mt) {
    if(mt && mt->mutex) {
        pthread_mutex_lock((pthread_mutex_t*)mt->mutex);
    }
}

void cMutexUnlockPort(MutexT * mt) {
    if(mt && mt->mutex) {
        pthread_mutex_unlock((pthread_mutex_t*)mt->mutex);
    }
}

void cMutexDestroyPort(MutexT *mt) {
    if(mt && mt->mutex) {
        pthread_mutex_destroy((pthread_mutex_t*)mt->mutex);
        pthread_mutexattr_destroy(&g_mutex_attr);
        cFree((void*)mt->mutex);
        mt->mutex = NULL;
    }
}

