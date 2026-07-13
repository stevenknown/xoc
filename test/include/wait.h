#ifndef _WAIT_H_
#define _WAIT_H_

int wait(int *status);
pid_t waitpid(pid_t pid, int *status, int options);

#endif
