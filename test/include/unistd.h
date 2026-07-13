#include "stddef.h"
#include "types.h"

#ifndef _UNISTD_H_
#define _UNISTD_H_

char *optarg;
int optind = 1;

char *getpass(const char *prompt);
int execvp(const char *file, char *const argv[]);
int isatty(int filedes);
int getopt(int argc, char *const argv[], const char *optstring);
pid_t getpid(void);
int execlp(const char *file, const char *arg0, ...);

#endif
