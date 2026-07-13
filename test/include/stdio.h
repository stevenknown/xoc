#include "stdlib.h"

#ifndef _STDIO_H_
#define _STDIO_H_

#include "stdarg.h"

/* Seek method constants */
#define SEEK_CUR    1
#define SEEK_END    2
#define SEEK_SET    0

#define EOF (-1L) 

typedef struct FILE FILE;
struct FILE;
extern int stderr;
extern int stdin;
extern int stdout;


void printf(char const*,...);
void fprintf(FILE *, char const*, ...);
int vfprintf(FILE *stream, const char *format, va_list args);
int vprintf(const char *format, va_list arg);
int sprintf(char *str, const char *format, ...);
int vsnprintf(char *str, size_t size, const char *format, va_list args);
FILE * fopen(char const*, char const*);
void fflush(FILE *);
void fclose(FILE *);
int ferror(FILE *);
char * fgets(char * s, int size, FILE * stream);
size_t fread( void *buffer, size_t size, size_t count, FILE *stream );
size_t fwrite(void *ptr, size_t size, size_t nmemb, FILE *stream);
int fseek(FILE *stream, long int offset, int whence);
FILE *open_memstream(char **ptr, size_t *sizeloc);
int fputc(int c, FILE *stream);
int unlink(const char *pathname);
int fileno(FILE *stream);
int getc(FILE *stream);
int fgetc(FILE *stream);
int feof(FILE *stream);
int ungetc(int c, FILE *stream);
void setbuf(FILE *stream, char *buf);
char *getcwd(char *buf, size_t size);

#endif
