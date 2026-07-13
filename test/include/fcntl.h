#ifndef _FCNTL_H_
#define _FCNTL_H_

int open(const char * pathname, int flags);
unsigned int read(int fd, void * buf, unsigned long count);
int close(int fd);

#endif
