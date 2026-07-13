#include "stdlib.h"

/* Seek method constants */
#define SEEK_CUR    1
#define SEEK_END    2
#define SEEK_SET    0

typedef struct FILE FILE;
struct FILE;
extern int stderr;
extern int stdin;
extern int stdout;

void printf(char const*,...);
void fprintf(FILE *, char const*, ...);
FILE * fopen(char const*, char const*);
void fflush(FILE *);
void fclose(FILE *);
int ferror(FILE *); 
char * fgets(char * s, int size, FILE * stream);
size_t fread( void *buffer, size_t size, size_t count, FILE *stream );
size_t fwrite(void *ptr, size_t size, size_t nmemb, FILE *stream);
int fseek(FILE *stream, long int offset, int whence);
