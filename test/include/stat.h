#ifndef _STAT_H_
#define _STAT_H_

#include "types.h"
#include "time.h"

struct stat {
    dev_t     st_dev;     /* ID of device containing file */
    ino_t     st_ino;     /* inode number */
    mode_t    st_mode;    /* protection */
    nlink_t   st_nlink;   /* number of hard links */
    uid_t     st_uid;     /* user ID of owner */
    gid_t     st_gid;     /* group ID of owner */
    dev_t     st_rdev;    /* ID of device (if special file) */
    off_t     st_size;    /* total size, in bytes */
    time_t    st_atime;   /* time of last access */
    time_t    st_mtime;   /* time of last modification */
    time_t    st_ctime;   /* time of last status change */
    blksize_t st_blksize; /* blocksize for filesystem I/O */
    blkcnt_t  st_blocks;  /* number of 512B blocks allocated */
};

//struct stat {
//  __dev_t st_dev;		/* Device.  */
//  unsigned short int __pad1;
//  __ino_t __st_ino;			/* 32bit file serial number.	*/
//  __mode_t st_mode;			/* File mode.  */
//  __nlink_t st_nlink;			/* Link count.  */
//  __uid_t st_uid;		/* User ID of the file's owner.	*/
//  __gid_t st_gid;		/* Group ID of the file's group.*/
//  __dev_t st_rdev;		/* Device number, if device.  */
//  unsigned short int __pad2;
//  __off64_t st_size;			/* Size of file, in bytes.  */
//  __blksize_t st_blksize;	/* Optimal block size for I/O.  */
//  __blkcnt64_t st_blocks;		/* Number 512-byte blocks allocated. */
//  __time_t st_atime;			/* Time of last access.  */
//  __syscall_ulong_t st_atimensec;	/* Nscecs of last access.  */
//  __time_t st_mtime;			/* Time of last modification.  */
//  __syscall_ulong_t st_mtimensec;	/* Nsecs of last modification.  */
//  __time_t st_ctime;			/* Time of last status change.  */
//  __syscall_ulong_t st_ctimensec;	/* Nsecs of last status change.  */
//  unsigned long int __unused4;
//  unsigned long int __unused5;
//};

int stat(const char * path, struct stat * buf);
int fstat(int fd, struct stat *buf);
int lstat(const char *path, struct stat *buf);
int fstatat(int filedes, const char *path, struct stat *buf, int flag);

#endif
