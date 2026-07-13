#ifndef _LOCALE_H_
#define _LOCALE_H_

#define LC_COLLATE (1 << 0)
#define LC_CTYPE (1 << 1)
#define LC_MONETARY (1 << 2)
#define LC_NUMERIC (1 << 3)
#define LC_TIME (1 << 4)
#define LC_MESSAGES (1 << 5)
#define LC_PAPER (1 << 6)
#define LC_NAME (1 << 7)
#define LC_ADDRESS (1 << 8)
#define LC_TELEPHONE (1 << 9)
#define LC_MEASUREMENT (1 << 10)
#define LC_IDENTIFICATION (1 << 11)

#define LC_ALL (LC_COLLATE | LC_CTYPE | LC_MONETARY | LC_NUMERIC | LC_TIME | LC_MESSAGES | LC_PAPER | LC_NAME | LC_ADDRESS | LC_TELEPHONE | LC_MEASUREMENT | LC_IDENTIFICATION)

char *setlocale(int category, const char *locale);
#endif
