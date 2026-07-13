#ifndef _TIME_H_
#define _TIME_H_

struct tm {
    int tm_sec;
    int tm_min;
    int tm_hour;
    int tm_mday;
    int tm_mon;
    int tm_year;
    int tm_wday;
    int tm_yday;
    int tm_isdst;
};

typedef long __time_t;
typedef __time_t time_t;
char *ctime_r(const time_t *timer, char *buf);
time_t time(time_t *t);
struct tm *localtime(const time_t *timer);
size_t strftime(char *s, size_t maxsize, const char *format,
                const struct tm *timeptr);
struct tm *localtime_r(const time_t *timep, struct tm *result);

#endif
