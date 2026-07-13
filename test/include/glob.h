#ifndef _ALLOCA_H_
#define _ALLOCA_H_

typedef struct {
    size_t gl_pathc;
    char **gl_pathv;
    size_t gl_off;
} glob_t;
int glob(const char *pattern, int flags,
         int (*errfunc)(const char *epath, int eerrno), glob_t *pglob);
void globfree(glob_t *pglob);

#endif
