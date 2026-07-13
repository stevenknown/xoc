#ifndef _TYPES_H_
#define _TYPES_H_

#include "stddef.h"

typedef unsigned int pid_t;
typedef unsigned long __dev_t;
typedef __dev_t dev_t;
typedef unsigned long __ino_t;
typedef __ino_t ino_t;
typedef unsigned short __mode_t;
typedef __mode_t mode_t;
typedef unsigned int __nlink_t;
typedef __nlink_t nlink_t;
typedef unsigned int __uid_t;
typedef __uid_t uid_t;
typedef unsigned int __gid_t;
typedef __gid_t gid_t;
typedef unsigned long off_t;
typedef off_t off64_t;
typedef off64_t __off64_t;
typedef size_t __blksize_t;
typedef __blksize_t blksize_t;
typedef unsigned long __blkcnt_t;
typedef __blkcnt_t blkcnt_t;
typedef unsigned long long __blkcnt64_t;
typedef __blkcnt64_t blkcnt64_t;
typedef unsigned long __syscall_ulong_t;
typedef __syscall_ulong_t syscall_ulong_t;

int fork(void);

#endif
