#ifndef _STDDEF_H_
#define _STDDEF_H_
typedef long unsigned int size_t;

#undef NULL
#if defined(__cplusplus)
    #define NULL 0
#else
    #define NULL ((void *)0)
#endif 

#endif
