#include "stddef.h"

#ifndef _STRING_H_
#define _STRING_H_

int strcpy(char*, char const*);
int strcmp(char*, char const*);
int strncmp(const char *s1, const char *s2, size_t n);
char *strdup(const char *s);
char *strndup(const char *s, size_t n);
char *strerror(int errnum);
char *strrchr(const char *str, int c);
char *strncpy(char *dest, const char *src, size_t n);

//Note strncasecmp is not the C99 standard api.
int strncasecmp(const char *s1, const char *s2, size_t n);
char *strchr(const char *haystack, int needle);
char *strstr(const char *haystack, const char *needle);
int strlen(char const*);
void * memcpy(void *dest, void *src, size_t count);
int memcmp(const void *str1, const void *str2, size_t n);
void *memset(void *s, int ch, size_t count);
size_t strcspn(const char *str1, const char *str2);
char *strtok(char *restrict s, const char *restrict delimiters);

#endif
