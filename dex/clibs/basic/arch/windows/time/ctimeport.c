/*Copyright 2011 Alibaba Group*/
#include <winsock2.h>
#include "time/ctimeport.h"

//for win32 platforms that need gettimeofday

/* FILETIME of Jan 1 1970 00:00:00. */
static const UInt64 epoch = LLValue(116444736000000000);

int gettimeofday(struct timeval * tp, struct timezone * tzp) {
    FILETIME    file_time;
    SYSTEMTIME    system_time;
    ULARGE_INTEGER ularge;

    GetSystemTime(&system_time);
    SystemTimeToFileTime(&system_time, &file_time);
    ularge.LowPart = file_time.dwLowDateTime;
    ularge.HighPart = file_time.dwHighDateTime;

    tp->tv_sec = (long) ((ularge.QuadPart - epoch) / 10000000L);
    tp->tv_usec = (long) (system_time.wMilliseconds * 1000);

    return 0;
}

static Float32 _sysTimeZone = 8.00;

UInt32 cTimeTicksPort(void) {
    return clock();
}

//get time in milliseconds since GMT 1970-01-01 00:00:00 000
CTimeT cTimeGetTimePort(void) {
    struct timeval tv;
    gettimeofday(&tv, NULL);
    return ((UInt64)tv.tv_sec)*1000+tv.tv_usec/1000;
}

//from -12.00 to +12.00
void cTimeSetTimeZonePort(Float32 tz) {
    _sysTimeZone = tz;
}

Float32 cTimeGetTimeZonePort(void) {
    return _sysTimeZone;
}
