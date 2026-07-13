typedef long int __fd_mask;
typedef struct
{
  __fd_mask fds_bits[1024 / (8 * (int) sizeof (__fd_mask))];
} fd_set;
