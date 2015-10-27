/*Copyright 2011 Alibaba Group*/
#include "time/ctimeport.h"

static const UInt16 _normal_mdays[] = {
    0,    //1,31
    31,    //2,28 + 1
    59,    //3,31
    90,    //4,30
    120,    //5,31
    151,    //6,30
    181,    //7,31
    212,    //8,31
    243,    //9,30
    273,    //10,31
    304,    //11,30
    334,    //12,31
    365,
    999,
    999
};

static const UInt16 _leap_mdays[] = {
    0,    //1,31
    31,    //2,29
    60,    //3,31
    91,    //4,30
    121,    //5,31
    152,    //6,30
    182,    //7,31
    213,    //8,31
    244,    //9,30
    274,    //10,31
    305,    //11,30
    335,    //12,31
    366,
    999,
    999
};


/**
* set the local timezone, this will affect cTimeGetLocalDate() and cTimeTime2LocalDate()
* @param z: timezone, values of [-12 to +12]
*/
void cTimeSetTimeZone(Float32 z) {
    cTimeSetTimeZonePort(z);
}

/**
* get the local timezone
*/
Float32 cTimeGetTimeZone() {
    return cTimeGetTimeZonePort();
}

/**
* get date of the specified timezone, if need GMT date, set param z to 0
*/
void cTimeGetDate(CDateT* d, Float32 z) {
    if(d) {
        CTimeT t = cTimeGetTimePort();
        cTimeTime2Date(t, d, z);
    }
}

/**
* get local date
*/
void cTimeGetLocalDate(CDateT* d) {
    cTimeGetDate(d, cTimeGetTimeZonePort());
}

/**
* get the gmt time
*/
CTimeT cTimeGetTime(void) {
    return cTimeGetTimePort();
}

/**
* get the gmt nano time
*/
CTimeT cTimeGetNanoTime(void) {
    return cTimeGetNanoTimePort();
}

UInt32 cTimeTicks(void) {
    return cTimeTicksPort();
}

/**
* convert one date to another date of a specified timezone
* @param d1: source date
* @param d2: dist date
* @param z: dist timezone
*/
void cTimeDateConvert(CDateT* d1, CDateT* d2, Float32 z) {
    if(d1 && d2) {
        CTimeT t = cTimeDate2Time(d1);
        cTimeTime2Date(t, d2, z);
    }
}

/**
* convert time to date of specified timezone, if need GMT date, set param z to 0
*/
void cTimeTime2Date(CTimeT t, CDateT* d, Float32 z) {
    if(t && d) {
        UInt16 nday,nyear,n;
        UInt32 secs = CTIME_SECOND(t)+(Int32)(z*3600);
        nday = secs/(24*3600) + 1;/*Modified by larry:20090223*/
        nyear = nday/365;
        while((n = cTimeGetDays((UInt16)(1970+nyear), (UInt8)1, (UInt8)1)) > nday) {
            nyear--;
        }
        d->year = 1970+nyear;
        nday -= n;
        d->yday = nday+1;
        n = cTimeGetMonthAndDay(d->year,d->yday);
        d->mon = n>>8;
        d->mday = n&0xff;
        d->wday = cTimeGetWeekDay(d->year,d->mon,d->mday);
        secs %= 24*3600;
        d->hour = secs/3600;
        d->min = (secs/60)%60;
        d->sec = secs%60;
        d->ms = CTIME_MS(t);
        d->zone = z;
    }
}

/**
* convert time to local date
*/
void cTimeTime2LocalDate(CTimeT t, CDateT* d) {
    cTimeTime2Date(t, d, cTimeGetTimeZonePort());
}

/**
* convert date to time
*/
CTimeT cTimeDate2Time(CDateT* d) {
    CTimeT t;
    if(d == NULL)
        return 0;

    t = ((cTimeGetDays(d->year,d->mon,d->mday)*24)+d->hour)*3600-(Int32)d->zone*3600
            + d->min * 60 + d->sec;
    t += t*1000+d->ms;
    return t;
}

bool cTimeIsLeapYear(UInt16 year) {
    Int32 n1,n2,n3;
    bool ret;
    n1 = year%4;
    n2 = year%400;
    n3 = year%100;
    ret = (!n2)||((!n1)&&n3);
    return ret;
}

bool cTimeIsDateValid(UInt16 year, UInt8 month, UInt8 mday) {
    const UInt16 * _mdays;
    if(month < 1 || month > 12)
        return false;
    _mdays = cTimeIsLeapYear(year) ? _leap_mdays : _normal_mdays;
    return mday > 0 && mday <= (_mdays[month]-_mdays[month-1]);
}
/**
 * get days since 1970-1-1, begin from 0
 * for example:
 *     cTimeGetDays(1970,1,2)  returns 1
 */
UInt32 cTimeGetDays(UInt16 year, UInt8 month, UInt8 mday) {
    UInt32 n = (year-2001)/100;
    n -= n/4;
    return (year-1970)*365 + ((year-1969)/4) - n + cTimeGetYearDay(year,month,mday)-1;
}

UInt16 cTimeGetYearDay(UInt16 year, UInt8 month, UInt8 mday) {
    return (cTimeIsLeapYear(year) ? _leap_mdays[month-1] : _normal_mdays[month-1]) + mday;
}

UInt8 cTimeGetWeekDay(UInt16 year, UInt8 month, UInt8 mday) {
    UInt8 wday;
    if (month < 3) {
        month += 13;
        year--;
    }
    else {
        month++;
    }
    wday = (mday + 26 * month / 10 + year + year / 4 - year / 100 + year / 400 + 6) % 7;
    wday = wday ? wday : 7;
    return wday;
}

UInt16 cTimeGetMonthAndDay(UInt16 year, UInt16 yday) {
    UInt16 mday,month;
    const UInt16 * _mdays = cTimeIsLeapYear(year) ? _leap_mdays : _normal_mdays;
    month = (yday+27)/28;
    while(yday <= _mdays[month-1]) {
        month--;
    }
    mday = yday-_mdays[month-1];
    return (month<<8) | mday;
}
